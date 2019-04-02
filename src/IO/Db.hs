{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IO.Db
    ( migrate
    , getRegistration
    , saveRegistration'
    , deleteRegistration
    , allRegistrations
    , allRegistrations'
    , allRegistrationsOrderedByName
    , DbParticipant(..)
    , DbId(..)
    , withConfig
    , Handle
    ) where

import Control.Exception (bracket)
import Control.Monad (void, forM_)
import Data.String (IsString(fromString))
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Prelude hiding (id)

import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Calendar (Day)
import Data.Aeson (encode, decode')

import Types

import qualified Domain.Participant as P
import qualified Domain.Registration as R
import qualified Domain.SharedTypes as DT

instance ToField P.Accommodation where
    toField P.Gym = toField ("gym" :: T.Text)
    toField P.Camping = toField ("camping" :: T.Text)
    toField P.SelfOrganized = toField ("selfOrganized" :: T.Text)
    toField P.Hostel = toField ("hostel" :: T.Text)

instance FromField P.Accommodation where
    fromField f bs = do
        res <- fromField f bs
        case res :: String of
            "gym" -> pure P.Gym
            "camping" -> pure P.Camping
            "selfOrganized" -> pure P.SelfOrganized
            "hostel" -> pure P.Hostel
            other -> returnError PSQL.ConversionFailed f $ "ConventionSleeping needs to be either gym, camping or other, it is " ++ other

instance FromRow P.ExistingParticipant where
    fromRow = do
        id_ <- DT.Id <$> field
        type_ <- field
        name <- DT.Name <$> field
        birthday <- DT.Birthday <$> field
        ticketId <- DT.Id <$> field
        let pI = P.PersonalInformation name birthday
        case type_ :: T.Text of
            "frisbee" -> do
                let ticket = P.ticketFromId ticketId
                sleeping <- field
                data' <- field
                let (Just parsedJson) = decode' data'
                pure $ P.Participant' id_ pI ticket (P.ForFrisbee sleeping parsedJson)
            "juggler" -> do
                let ticket = P.ticketFromId ticketId
                sleeping <- field
                _ <- field :: RowParser (Maybe T.Text) -- ignoring data attribut
                pure $ P.Participant' id_ pI ticket (P.ForJuggler sleeping)
            _ -> fail "type_ must be either frisbee or juggler"

-- TODO: Do not expose this datatype, but parameterize the id + registeredAt field of the on in Types
-- e.g. Participant () () would come from the form
data DbParticipant = DbParticipant
    { dbParticipantId :: DbId DbParticipant
    , dbParticipantName :: T.Text
    , dbParticipantBirthday :: Day
    , dbParticipantStreet :: T.Text
    , dbParticipantPostalCode :: T.Text
    , dbParticipantCity :: T.Text
    , dbParticipantCountry :: T.Text
    , dbParticipantRegisteredAt :: UTCTime
    , dbParticipantSleepovers :: Sleepover
    , dbParticipantComment :: Maybe T.Text
    , dbParticipantEmail :: Maybe T.Text
    } deriving (Show)

newtype DbId a = DbId Int deriving (Show)

instance FromField (DbId a) where
    fromField f bs = DbId <$> fromField f bs

instance FromField Sleepover where
    fromField f bs = do
        value <- fromField f bs
        case value :: T.Text of
            -- both, fr and sa are for backwards compability only.
            -- We used to save the specific day on which the participant slept
            -- at the convention.
            "both" -> return GymSleeping
            "fr" -> return GymSleeping
            "sa" -> return GymSleeping
            "none" -> return NoNights
            "n/a" -> return CouldntSelect
            "camping" -> return Camping
            "gym" -> return GymSleeping
            _ -> fail "sleepover not of expected value"

instance FromRow DbParticipant where
    fromRow = DbParticipant <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Handle = Handle { _hHandle :: Pool.Pool PSQL.Connection }

withConfig :: String -> (Handle -> IO a) -> IO a
withConfig url f = do
    bracket
      (Pool.createPool (PSQL.connectPostgreSQL (BS.pack url)) (\c -> PSQL.close c) 1 30 5)
      Pool.destroyAllResources
      (\pool -> f (Handle pool))

saveRegistration' :: Handle -> R.NewRegistration -> IO DT.Id
saveRegistration' (Handle pool) R.Registration{..} =
    Pool.withResource pool $ \conn -> do
        PSQL.withTransaction conn $ do
            t <- getCurrentTime
            [PSQL.Only registrationId] <- PSQL.query conn "INSERT INTO registrations (email, paymentCode, comment, registeredAt) VALUES (?, ?, ?, ?) RETURNING id" (email, "0" :: String, comment, t)
            _ <- PSQL.execute conn "UPDATE registrations SET paymentCode = ? WHERE id = ?" (show $ registrationId + 100, registrationId)
            forM_ participants $ \p -> do
                case p of
                    P.Participant' () (P.PersonalInformation (DT.Name name) (DT.Birthday birthday)) (P.Ticket (DT.Id ticketId) _ _ _) (P.ForJuggler sleeping) ->
                        PSQL.execute conn "INSERT INTO participants (name, birthday, registrationId, accommodation, ticketId, type) VALUES (?, ?, ?, ?, ?, ?)" (name, birthday, registrationId :: Int, sleeping, ticketId, "juggler" :: T.Text)
                    P.Participant' () (P.PersonalInformation (DT.Name name) (DT.Birthday birthday)) (P.Ticket (DT.Id ticketId) _ _ _) (P.ForFrisbee sleeping data') -> do
                        PSQL.execute conn "INSERT INTO participants (name, birthday, registrationId, accommodation, ticketId, type, frisbeeDetails) VALUES (?, ?, ?, ?, ?, ?, ?)" (name, birthday, registrationId :: Int, sleeping, ticketId, "frisbee" :: T.Text, encode data')
            pure $ DT.Id registrationId

getRegistration :: Handle -> DT.Id -> IO R.ExistingRegistration
getRegistration (Handle pool) (DT.Id registrationId) = do
    Pool.withResource pool $ \conn -> do
        [(id, email, paymentCode, comment, registeredAt)] <- PSQL.query conn "SELECT id, email, paymentCode, comment, registeredAt FROM registrations WHERE id = ?" (PSQL.Only registrationId)
        participants <- PSQL.query conn "SELECT id, type, name, birthday, ticketId, accommodation, frisbeeDetails FROM participants WHERE registrationId = ?" (PSQL.Only id)
        pure $ R.Registration (DT.Id id) email (NE.fromList participants) comment (DT.PaymentCode paymentCode) registeredAt


deleteRegistration :: Handle -> DbId Participant -> IO ()
deleteRegistration (Handle pool) (DbId id') =
    Pool.withResource pool $ \conn -> do
        void $ PSQL.execute conn "DELETE FROM registrations WHERE id = ?" (PSQL.Only id')
        void $ PSQL.execute conn "DELETE FROM participants WHERE registrationId = ?" (PSQL.Only id')

allRegistrations' :: Handle -> IO [R.ExistingRegistration]
allRegistrations' handle@(Handle pool) = do
    Pool.withResource pool $ \conn -> do
        registrationIds <- PSQL.query_ conn "SELECT id FROM registrations"
        mapM (getRegistration handle) ((\(PSQL.Only id) -> DT.Id id) <$> registrationIds)


allRegistrations :: Handle -> IO [DbParticipant]
allRegistrations (Handle pool) =
    Pool.withResource pool $ \conn -> do
        PSQL.query_ conn "SELECT id, name, birthday, street, postalCode, city, country, registeredAt, sleepovers, comment, email FROM participants ORDER BY registeredAt DESC"

allRegistrationsOrderedByName :: Handle -> IO [DbParticipant]
allRegistrationsOrderedByName (Handle pool) = do
    Pool.withResource pool $ \conn -> do
        PSQL.query_ conn "SELECT id, name, birthday, street, postalCode, city, country, registeredAt, sleepovers, comment, email FROM participants ORDER BY name"


migrate :: Handle -> IO ()
migrate (Handle pool) =
    Pool.withResource pool $ \conn ->
        void $ PSQL.execute_ conn statement
  where
    statement = fromString $ unlines
        [ "CREATE TABLE IF NOT EXISTS participants ("
        , "id SERIAL PRIMARY KEY,"
        , "name text NOT NULL,"
        , "birthday date NOT NULL,"
        , "registrationId int NOT NULL,"
        , "accommodation text NOT NULL,"
        , "type text NOT NULL,"
        , "ticketId int NOT NULL,"
        , "frisbeeDetails text);"
        -- TODO: Ticket => Map (,) to two columns

        , "CREATE TABLE IF NOT EXISTS registrations ("
        , "id SERIAL PRIMARY KEY,"
        , "email text NOT NULL,"
        , "paymentCode text NOT NULL,"
        , "comment text,"
        , "registeredAt timestamptz NOT NULL);"
        ]
