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
  , Price
  , ticketChoices
  , ticketFromId
  , ageLabel
  , stayLabel
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
participantName p =
    case p of
        (FrisbeeParticipant _ pI _ _) -> name pI
        (JugglingParticipant _ pI _ _) -> name pI

participantTicket :: Participant' status -> Ticket
participantTicket p =
    case p of
        (FrisbeeParticipant _ _ t _) -> t
        (JugglingParticipant _ _ t _) -> t

data Participant' status
    = FrisbeeParticipant (MaybePersisted status Id) PersonalInformation Ticket (Either Hostel ConventionSleeping)
    | JugglingParticipant (MaybePersisted status Id) PersonalInformation Ticket ConventionSleeping

type NewParticipant = Participant' 'New
type ExistingParticipant = Participant' 'Persisted

instance Show NewParticipant where
    show (FrisbeeParticipant id_ pI ticket sleeping) = "Frisbee " ++ show id_ ++ show pI ++ show sleeping ++ show ticket
    show (JugglingParticipant id_ pI ticket sleeping) = "Juggler " ++ show id_ ++ show pI ++ show sleeping ++ show ticket

instance Show ExistingParticipant where
    show (FrisbeeParticipant id_ pI ticket sleeping) = "Frisbee " ++ show id_ ++ show pI ++ show sleeping ++ show ticket
    show (JugglingParticipant id_ pI ticket sleeping) = "Juggler " ++ show id_ ++ show pI ++ show sleeping ++ show ticket


data Stay = LongStay | ShortStay deriving (Show, Eq)
data AgeCategory = Baby | Child | OlderThan12 deriving (Show, Eq)

data Ticket = Ticket { id :: Id, ageCategory :: AgeCategory, stay :: Stay, price :: Int } deriving Show

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

newtype Price = Price Int

instance Show Price where
    show (Price x)
        | x == 0 = "Kostenlos"
        | otherwise = show x ++ "€"

-- Make sure to not remove any tickets once this is live.
-- Also: Tag each ticket with either juggler or frisbee to not mix them up
ticketChoices :: [Ticket]
ticketChoices =
    [ Ticket (Id 1) OlderThan12 LongStay 39
    , Ticket (Id 2) Child LongStay 20
    , Ticket (Id 3) Baby LongStay 0
    , Ticket (Id 4) OlderThan12 ShortStay 30
    , Ticket (Id 5) Child ShortStay 15
    , Ticket (Id 6) Baby ShortStay 0
    ]

ticketFromId :: Id -> Ticket
ticketFromId id' = head $ filter (\(Ticket id'' _ _ _) -> id'' == id') ticketChoices
