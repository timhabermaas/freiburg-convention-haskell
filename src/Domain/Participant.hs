{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Participant
  ( Accommodation(..)
  , Participant'(..)
  , ExistingParticipant
  , NewParticipant
  , PersonalInformation(..)
  , participantName
  , participantBirthday
  , participantTicket
  , Ticket(..)
  , ticketLabel
  , defaultTicket
  , Stay(..)
  , AgeCategory(..)
  , jugglerTicketChoices
  , frisbeeTicketChoices
  , ticketFromId
  , ageLabel
  , stayLabel
  , ParticipantDetail(..)
  , FrisbeeDetail(..)
  , isJuggler
  , isFrisbee
  ) where

import Domain.SharedTypes
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Prelude hiding (id)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar (Day)

data PersonalInformation = PersonalInformation
  { name :: Name
  , birthday :: Birthday
  } deriving Show

data Accommodation = Gym | Camping | SelfOrganized | Hostel deriving (Show, Eq)

participantName :: Participant' status -> Name
participantName (Participant' _ pI _ _) = name pI

participantBirthday :: Participant' status -> Birthday
participantBirthday (Participant' _ pI _ _) = birthday pI

participantTicket :: Participant' status -> Ticket
participantTicket (Participant' _ _ t _) = t

data FrisbeeDetail
    = FrisbeeDetail
    { city :: City
    , country :: Country
    , phoneNumber :: PhoneNumber
    , divisionParticipation :: Set.Set Division -- Should actually be a non empty set
    , partnerOpenPairs :: Maybe (Partner 'OpenPairs)
    , partnerOpenCoop :: Maybe (Partner 'OpenCoop)
    , partnerMixedPairs :: Maybe (Partner 'MixedPairs)
    , lookingForPartner :: Set.Set Division
    , arrival :: Day
    , departure :: Day
    } deriving (Show, Generic)

instance ToJSON FrisbeeDetail
instance FromJSON FrisbeeDetail

data ParticipantDetail
    = ForFrisbee Accommodation FrisbeeDetail
    | ForJuggler Accommodation
    deriving (Show, Generic)

data Participant' status
    = Participant' (MaybePersisted status Id) PersonalInformation Ticket ParticipantDetail

type NewParticipant = Participant' 'New
type ExistingParticipant = Participant' 'Persisted

instance Show NewParticipant where
    show (Participant' id_ pI ticket details) = show id_ ++ show pI ++ show details ++ show ticket

instance Show ExistingParticipant where
    show (Participant' id_ pI ticket details) = show id_ ++ show pI ++ show details ++ show ticket


data Stay = LongStay | ShortStay deriving (Show, Eq)
data AgeCategory = Baby | Child | OlderThan12 deriving (Show, Eq)

data Ticket = Ticket { id :: Id, ageCategory :: AgeCategory, stay :: Stay, price :: Price } deriving Show

ticketLabel :: Ticket -> T.Text
ticketLabel Ticket{..} = stayLabel stay <> ", " <> ageLabel ageCategory <> ": " <> priceLabel price
  where
    priceLabel price_ = T.pack $ show price_

ageLabel :: AgeCategory -> T.Text
ageLabel Baby = "0–3 Jahre"
ageLabel Child = "4–12 Jahre"
ageLabel OlderThan12 = ">12 Jahre"

stayLabel :: Stay -> T.Text
stayLabel LongStay = "Do.–So."
stayLabel ShortStay = "Fr.–So."

instance Eq Ticket where
    t1 == t2 = id t1 == id t2

defaultTicket :: Ticket
defaultTicket = head jugglerTicketChoices


-- Make sure to not remove any tickets once this is live.
-- Also: Tag each ticket with either juggler or frisbee to not mix them up
jugglerTicketChoices :: [Ticket]
jugglerTicketChoices =
    [ Ticket (Id 1) OlderThan12 LongStay (Price 39)
    , Ticket (Id 2) Child LongStay (Price 20)
    , Ticket (Id 3) Baby LongStay (Price 0)
    , Ticket (Id 4) OlderThan12 ShortStay (Price 30)
    , Ticket (Id 5) Child ShortStay (Price 15)
    , Ticket (Id 6) Baby ShortStay (Price 0)
    ]

frisbeeTicketChoices :: [Ticket]
frisbeeTicketChoices =
    [ Ticket (Id 7) OlderThan12 LongStay (Price 44)
    , Ticket (Id 8) Child LongStay (Price 25)
    , Ticket (Id 9) Baby LongStay (Price 0)
    , Ticket (Id 10) OlderThan12 ShortStay (Price 35)
    , Ticket (Id 11) Child ShortStay (Price 20)
    , Ticket (Id 12) Baby ShortStay (Price 0)
    ]

allTicketChoices :: [Ticket]
allTicketChoices = jugglerTicketChoices ++ frisbeeTicketChoices

ticketFromId :: Id -> Ticket
ticketFromId id' = head $ filter (\(Ticket id'' _ _ _) -> id'' == id') allTicketChoices

isJuggler :: Participant' s -> Bool
isJuggler (Participant' _ _ _ (ForJuggler _)) = True
isJuggler _ = False

isFrisbee :: Participant' s -> Bool
isFrisbee (Participant' _ _ _ (ForFrisbee _ _)) = True
isFrisbee _ = False
