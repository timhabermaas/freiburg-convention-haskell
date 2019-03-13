{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IO.Db
    ( migrate
    , saveRegistration
    , deleteRegistration
    , allRegistrations
    , allRegistrationsOrderedByName
    , DbParticipant(..)
    , DbId(..)
    , withConfig
    , Handle
    ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.String (IsString(fromString))
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Calendar (Day)

import Types

import qualified Domain.Participant as P
import qualified Domain.SharedTypes as DT

instance FromField P.ConventionSleeping where
    fromField f bs = do
        res <- fromField f bs
        case res :: String of
            "gym" -> pure P.Gym
            "camping" -> pure P.Camping
            "other" -> pure P.SelfOrganized
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
        let pI = P.PersonalInformation name birthday
        case type_ :: T.Text of
            "frisbee" -> do
                sleeping <- field
                pure $ P.FrisbeeParticipant id_ pI undefined sleeping
            "juggler" -> do
                sleeping <- field
                pure $ P.JugglingParticipant id_ pI undefined sleeping
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

saveRegistration :: Handle -> Participant -> IO ()
saveRegistration (Handle pool) Participant{..} =
    Pool.withResource pool $ \conn -> do
        t <- getCurrentTime
        void $ PSQL.execute conn "INSERT INTO participants (name, birthday, street, postalCode, city, country, registeredAt, sleepovers, comment, email) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (participantName, participantBirthday, participantStreet, participantPostalCode, participantCity, participantCountry, t, sleepoversToText participantSleepovers, participantComment, participantEmail)

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
        , "sleepovers text NOT NULL,"
        , "registeredAt timestamptz NOT NULL,"
        , "comment text);"
        ]
