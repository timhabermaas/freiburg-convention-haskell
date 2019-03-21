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
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Calendar (Day)

import Types

import qualified Domain.Participant as P
import qualified Domain.Registration as R
import qualified Domain.SharedTypes as DT

instance FromField P.ConventionSleeping where
    fromField f bs = do
        res <- fromField f bs
        case res :: String of
            "gym" -> pure P.Gym
            "camping" -> pure P.Camping
            "selfOrganized" -> pure P.SelfOrganized
            _ -> fail "ConventionSleeping needs to be either gym, camping or other"

instance FromField P.Hostel where
    fromField f bs = do
        s <- fromField f bs
        if (s :: T.Text) == "hostel" then
            pure P.Hostel
        else
            fail "Can't parse Hostel"

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
                sleeping <- field
                -- TODO: Grab ticket from frisbee tickets
                pure $ P.FrisbeeParticipant id_ pI undefined sleeping
            "juggler" -> do
                let ticket = P.ticketFromId ticketId
                sleeping <- field
                pure $ P.JugglingParticipant id_ pI ticket sleeping
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

sleepoversToText :: Sleepover -> T.Text
sleepoversToText NoNights = "none"
sleepoversToText CouldntSelect = "n/a"
sleepoversToText Camping = "camping"
sleepoversToText GymSleeping = "gym"

instance FromRow DbParticipant where
    fromRow = DbParticipant <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Handle = Handle { _hHandle :: Pool.Pool PSQL.Connection }

withConfig :: String -> (Handle -> IO a) -> IO a
withConfig url f = do
    bracket
      (Pool.createPool (PSQL.connectPostgreSQL (BS.pack url)) (\c -> PSQL.close c) 1 30 5)
      (\pool -> Pool.destroyAllResources pool)
      (\pool -> f (Handle pool))

accommodationToText :: P.ConventionSleeping -> T.Text
accommodationToText P.Gym = "gym"
accommodationToText P.Camping = "camping"
accommodationToText P.SelfOrganized = "selfOrganized"

accommodationFromText :: T.Text -> P.ConventionSleeping
accommodationFromText t
    | t == "gym" = P.Gym
    | t == "camping" = P.Camping
    | t == "selfOrganized" = P.SelfOrganized
    | otherwise = error "couldn't parse accomodation"

saveRegistration' :: Handle -> R.NewRegistration -> IO DT.Id
saveRegistration' (Handle pool) R.Registration{..} =
    Pool.withResource pool $ \conn -> do
        PSQL.withTransaction conn $ do
            t <- getCurrentTime
            [PSQL.Only registrationId] <- PSQL.query conn "INSERT INTO registrations (email, paymentCode, comment, registeredAt) VALUES (?, ?, ?, ?) RETURNING id" (email, "0" :: String, comment, t)
            PSQL.execute conn "UPDATE registrations SET paymentCode = ? WHERE id = ?" (show $ registrationId + 100, registrationId)
            forM_ participants $ \p -> do
                case p of
                    P.JugglingParticipant () (P.PersonalInformation (DT.Name name) (DT.Birthday birthday)) (P.Ticket (DT.Id ticketId) _ _ _) sleeping ->
                        -- TODO: Use ToRow instance
                        PSQL.execute conn "INSERT INTO participants (name, birthday, registrationId, accommodation, ticketId, type) VALUES (?, ?, ?, ?, ?, ?)" (name, birthday, registrationId :: Int, accommodationToText sleeping, ticketId, "juggler" :: T.Text)
            pure $ DT.Id registrationId

getRegistration :: Handle -> DT.Id -> IO R.ExistingRegistration
getRegistration (Handle pool) (DT.Id id) = do
    Pool.withResource pool $ \conn -> do
        [(id, email, paymentCode, comment, registeredAt)] <- PSQL.query conn "SELECT id, email, paymentCode, comment, registeredAt FROM registrations WHERE id = ?" (PSQL.Only id)
        participants <- PSQL.query conn "SELECT id, type, name, birthday, ticketId, accommodation FROM participants WHERE registrationId = ?" (PSQL.Only id)
        pure $ R.Registration (DT.Id id) email participants comment (DT.PaymentCode paymentCode) registeredAt


deleteRegistration :: Handle -> DbId Participant -> IO ()
deleteRegistration (Handle pool) (DbId id') =
    Pool.withResource pool $ \conn -> do
        void $ PSQL.execute conn "DELETE FROM participants WHERE id = ?" (PSQL.Only id')


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
        , "ticketId int NOT NULL);"
        -- TODO: Ticket => Map (,) to two columns

        , "CREATE TABLE IF NOT EXISTS registrations ("
        , "id SERIAL PRIMARY KEY,"
        , "email text NOT NULL,"
        , "paymentCode text NOT NULL,"
        , "comment text,"
        , "registeredAt timestamptz NOT NULL);"
        ]
