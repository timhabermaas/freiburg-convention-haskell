{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Domain.Participant
  ( ConventionSleeping(..)
  , Hostel(..)
  , Participant'(..)
  , ExistingParticipant
  , NewParticipant
  , PersonalInformation(..)
  , participantName
  , participantTicket
  , Ticket(..)
  , defaultTicket
  , Stay(..)
  , AgeCategory(..)
  , ticketChoices
  , ticketFromId
  , ageLabel
  , stayLabel
  , ParticipantDetail(..)
  ) where

import Domain.SharedTypes
import Prelude hiding (id)
import qualified Data.Text as T

data PersonalInformation = PersonalInformation
  { name :: Name
  , birthday :: Birthday
  } deriving Show

data ConventionSleeping = Gym | Camping | SelfOrganized deriving (Show, Eq)
data Hostel = Hostel deriving Show

participantName :: Participant' status -> Name
participantName (Participant' _ pI _ _) = name pI

participantTicket :: Participant' status -> Ticket
participantTicket (Participant' _ _ t _) = t

data ParticipantDetail
    = FrisbeeDetail (Either Hostel ConventionSleeping)
    | JugglerDetail ConventionSleeping
    deriving Show

data Participant' status
    = Participant' (MaybePersisted status Id) PersonalInformation Ticket ParticipantDetail
    -- = FrisbeeParticipant (MaybePersisted status Id) PersonalInformation Ticket (Either Hostel ConventionSleeping)
    -- | JugglingParticipant (MaybePersisted status Id) PersonalInformation Ticket ConventionSleeping

type NewParticipant = Participant' 'New
type ExistingParticipant = Participant' 'Persisted

instance Show NewParticipant where
    show (Participant' id_ pI ticket details) = show id_ ++ show pI ++ show details ++ show ticket

instance Show ExistingParticipant where
    show (Participant' id_ pI ticket details) = show id_ ++ show pI ++ show details ++ show ticket


data Stay = LongStay | ShortStay deriving (Show, Eq)
data AgeCategory = Baby | Child | OlderThan12 deriving (Show, Eq)

data Ticket = Ticket { id :: Id, ageCategory :: AgeCategory, stay :: Stay, price :: Price } deriving Show

ageLabel :: AgeCategory -> T.Text
ageLabel Baby = "0–3 Jahre"
ageLabel Child = "4–12 Jahre"
ageLabel OlderThan12 = ">12 Jahre"

stayLabel :: Stay -> T.Text
stayLabel LongStay = "Do.–So. inkl. Shows"
stayLabel ShortStay = "Fr.–So. inkl. Shows"

instance Eq Ticket where
    t1 == t2 = id t1 == id t2

defaultTicket :: Ticket
defaultTicket = head ticketChoices


-- Make sure to not remove any tickets once this is live.
-- Also: Tag each ticket with either juggler or frisbee to not mix them up
ticketChoices :: [Ticket]
ticketChoices =
    [ Ticket (Id 1) OlderThan12 LongStay (Price 39)
    , Ticket (Id 2) Child LongStay (Price 20)
    , Ticket (Id 3) Baby LongStay (Price 0)
    , Ticket (Id 4) OlderThan12 ShortStay (Price 30)
    , Ticket (Id 5) Child ShortStay (Price 15)
    , Ticket (Id 6) Baby ShortStay (Price 0)
    ]

ticketFromId :: Id -> Ticket
ticketFromId id' = head $ filter (\(Ticket id'' _ _ _) -> id'' == id') ticketChoices
