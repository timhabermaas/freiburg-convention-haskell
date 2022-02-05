{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IO.Db
  ( migrate,
    getRegistration,
    saveRegistration',
    deleteRegistration,
    payRegistration,
    allParticipantsWithRegistration,
    allParticipants,
    allRegistrations',
    DbId (..),
    withConfig,
    Handle,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Pool as Pool
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import qualified Domain.Participant as P
import qualified Domain.Registration as R
import qualified Domain.SharedTypes as DT
import Types
import Prelude hiding (id)

data ParticipantType = Juggler deriving (Show)

instance FromField ParticipantType where
  fromField f bs = do
    res <- fromField @String f bs
    case res of
      "juggler" -> pure Juggler
      other -> returnError PSQL.ConversionFailed f $ "Type must be constant juggler, but is " ++ other

instance ToField P.Accommodation where
  toField P.Gym = toField ("gym" :: T.Text)
  toField P.Camping = toField ("camping" :: T.Text)
  toField P.SelfOrganized = toField ("selfOrganized" :: T.Text)

instance FromField P.Accommodation where
  fromField f bs = do
    res <- fromField f bs
    case res :: String of
      "gym" -> pure P.Gym
      "camping" -> pure P.Camping
      "selfOrganized" -> pure P.SelfOrganized
      other -> returnError PSQL.ConversionFailed f $ "ConventionSleeping needs to be either gym, camping or other, it is " ++ other

instance FromRow P.ExistingParticipant where
  fromRow = do
    id_ <- DT.Id <$> field
    _type <- field @ParticipantType
    name <- DT.Name <$> field
    birthday <- DT.Birthday <$> field
    ticketId <- DT.Id <$> field

    street <- field
    postalCode <- field
    city <- field
    country <- field
    sleeping <- field

    let pI = P.PersonalInformation name birthday
    let address = P.Address street postalCode city country
    let ticket = P.ticketFromId ticketId
    pure $ P.Participant' id_ pI address ticket sleeping

-- TODO: Do not expose this datatype, but parameterize the id + registeredAt field of the on in Types
-- e.g. Participant () () would come from the form
data DbParticipant = DbParticipant
  { dbParticipantId :: DbId DbParticipant,
    dbParticipantName :: T.Text,
    dbParticipantBirthday :: Day,
    dbParticipantStreet :: T.Text,
    dbParticipantPostalCode :: T.Text,
    dbParticipantCity :: T.Text,
    dbParticipantCountry :: T.Text,
    dbParticipantRegisteredAt :: UTCTime,
    dbParticipantSleepovers :: Sleepover,
    dbParticipantComment :: Maybe T.Text,
    dbParticipantEmail :: Maybe T.Text
  }
  deriving (Show)

newtype DbId a = DbId Int deriving (Show)

instance FromField (DbId a) where
  fromField f bs = DbId <$> fromField f bs

instance FromField Sleepover where
  fromField f bs = do
    value <- fromField @T.Text f bs
    case value of
      "none" -> pure NoNights
      "n/a" -> pure CouldntSelect
      "camping" -> pure Camping
      "gym" -> pure GymSleeping
      other -> returnError PSQL.ConversionFailed f $ show $ "sleepover not of expected value, found " <> other

instance FromRow DbParticipant where
  fromRow = DbParticipant <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

--data Handle = Handle { _hHandle :: Pool.Pool PSQL.Connection }
newtype Handle = Handle {_hHandle :: Pool.Pool PSQL.Connection}

withConfig :: String -> (Handle -> IO a) -> IO a
withConfig url f = do
  bracket
    (Pool.createPool (PSQL.connectPostgreSQL (BS.pack url)) PSQL.close 1 30 5)
    Pool.destroyAllResources
    (f . Handle)

saveRegistration' :: Handle -> R.NewRegistration -> IO DT.Id
saveRegistration' (Handle pool) R.Registration {..} =
  Pool.withResource pool $ \conn -> do
    PSQL.withTransaction conn $ do
      t <- getCurrentTime
      [PSQL.Only registrationId] <- PSQL.query conn "INSERT INTO registrations (email, paymentCode, comment, registeredAt) VALUES (?, ?, ?, ?) RETURNING id" (email, "0" :: String, comment, t)
      _ <- PSQL.execute conn "UPDATE registrations SET paymentCode = ? WHERE id = ?" (show $ registrationId + 100, registrationId)
      forM_ participants $ \p -> do
        case p of
          P.Participant' () (P.PersonalInformation (DT.Name name) (DT.Birthday birthday)) (P.Address street postalCode city country) (P.Ticket (DT.Id ticketId) _ _ _ _) sleeping ->
            PSQL.execute conn "INSERT INTO participants (name, birthday, registrationId, accommodation, ticketId, type, street, postalCode, city, country) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (name, birthday, registrationId :: Int, sleeping, ticketId, "juggler" :: T.Text, street, postalCode, city, country)
      pure $ DT.Id registrationId

getRegistration :: Handle -> DT.Id -> IO R.ExistingRegistration
getRegistration (Handle pool) (DT.Id registrationId) = do
  Pool.withResource pool $ \conn -> do
    [(id, email, paymentCode, comment, registeredAt, paidAt)] <- PSQL.query conn "SELECT id, email, paymentCode, comment, registeredAt, paidAt FROM registrations WHERE id = ?" (PSQL.Only registrationId)
    participants <- PSQL.query conn "SELECT id, type, name, birthday, ticketId, street, postalCode, city, country, accommodation FROM participants WHERE registrationId = ?" (PSQL.Only id)
    pure $ R.Registration (DT.Id id) email (NE.fromList participants) comment (DT.PaymentCode paymentCode) registeredAt (DT.paidStatusFromMaybeTime paidAt)

deleteRegistration :: Handle -> DbId Participant -> IO ()
deleteRegistration (Handle pool) (DbId id') =
  Pool.withResource pool $ \conn -> do
    void $ PSQL.execute conn "DELETE FROM registrations WHERE id = ?" (PSQL.Only id')
    void $ PSQL.execute conn "DELETE FROM participants WHERE registrationId = ?" (PSQL.Only id')

payRegistration :: Handle -> DbId R.ExistingRegistration -> IO ()
payRegistration (Handle pool) (DbId id') = do
  t <- getCurrentTime
  Pool.withResource pool $ \conn -> do
    void $ PSQL.execute conn "UPDATE registrations SET paidAt = ? WHERE id = ?" (t, id')

allParticipantsWithRegistration :: Handle -> IO [(P.ExistingParticipant, R.ExistingRegistration)]
allParticipantsWithRegistration handle = do
  registrations <- allRegistrations' handle
  let f a r@R.Registration {..} = fmap (\p -> (p, r)) (NE.toList participants) ++ a
  let result = L.foldl' f [] registrations
  pure $ L.sortOn (P.participantName . fst) result

allRegistrations' :: Handle -> IO [R.ExistingRegistration]
allRegistrations' handle@(Handle pool) = do
  Pool.withResource pool $ \conn -> do
    registrationIds <- PSQL.query_ conn "SELECT id FROM registrations ORDER BY registeredAt DESC"
    mapM (getRegistration handle) ((\(PSQL.Only id) -> DT.Id id) <$> registrationIds)

allParticipants :: Handle -> IO [P.ExistingParticipant]
allParticipants (Handle pool) = do
  Pool.withResource pool $ \conn ->
    PSQL.query_ conn "SELECT id, type, name, birthday, ticketId, street, postalCode, city, country, accommodation FROM participants"

migrate :: Handle -> IO ()
migrate (Handle pool) =
  Pool.withResource pool $ \conn ->
    void $ PSQL.execute_ conn statement
  where
    statement =
      fromString $
        unlines
          [ "CREATE TABLE IF NOT EXISTS participants (",
            "id SERIAL PRIMARY KEY,",
            "name text NOT NULL,",
            "birthday date NOT NULL,",
            "street text NOT NULL,",
            "postalCode text NOT NULL,",
            "city text NOT NULL,",
            "country text NOT NULL,",
            "registrationId int NOT NULL,",
            "accommodation text NOT NULL,",
            "type text NOT NULL,",
            "ticketId int NOT NULL);",
            "CREATE TABLE IF NOT EXISTS registrations (",
            "id SERIAL PRIMARY KEY,",
            "email text NOT NULL,",
            "paymentCode text NOT NULL,",
            "comment text,",
            "registeredAt timestamptz NOT NULL);",
            "ALTER TABLE registrations ADD COLUMN IF NOT EXISTS paidAt timestamptz;"
          ]
