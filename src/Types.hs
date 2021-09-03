{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( Participant(..)
    , Sleepover(..)
    , ParticipantId(..)
    , GymSleepingLimit(..)
    , CampingSleepingLimit(..)
    , LimitReached(..)
    , OverallLimit(..)
    , ParticipantLimits(..)
    ) where

import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Web.HttpApiData (FromHttpApiData)

-- CouldntSelect represents a participant which registered when all sleepover spots were already taken.
data Sleepover = Camping | NoNights | GymSleeping | CouldntSelect deriving (Show, Ord, Eq)

newtype ParticipantId = ParticipantId Int deriving (FromHttpApiData, Show)

data Participant = Participant
    { participantName :: T.Text
    , participantBirthday :: Day
    , participantStreet :: T.Text
    , participantPostalCode :: T.Text
    , participantCity :: T.Text
    , participantSleepovers :: Sleepover
    , participantCountry :: T.Text
    , participantComment :: Maybe T.Text
    , participantEmail :: Maybe T.Text
    } deriving (Show)

data LimitReached
  -- | All (sleeping) spots still available
  = NoLimitReached
  -- | No more camping spots, sleeping at gym still possible
  | CampingLimitReached
  -- | No more gym spots, sleeping in tent still possible
  | GymLimitReached
  -- | No more sleeping spots at side (neither gym nor tent), participant needs
  -- to sleep somewhere else
  | SleepingAtSideLimitReached
  -- | No more participants allowed, sleeping spots might still be available, but can't be used
  | OverallLimitReached
  deriving (Show, Eq)

newtype GymSleepingLimit = GymSleepingLimit Int deriving Show
newtype CampingSleepingLimit = CampingSleepingLimit Int deriving Show
newtype OverallLimit = OverallLimit Int deriving Show

data ParticipantLimits = ParticipantLimits
  { overallLimit :: OverallLimit
  , gymSleeping :: GymSleepingLimit
  , campingSleeping :: CampingSleepingLimit
  } deriving (Show)
