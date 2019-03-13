{-# LANGUAGE KindSignatures #-}
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
  , Ticket
  , defaultTicket
  , Stay(..)
  , AgeCategory(..)
  , Price
  , ticketPrice
  , ticketChoices
  ) where

import Domain.SharedTypes

data PersonalInformation = PersonalInformation
  { name :: Name
  , birthday :: Birthday
  } deriving Show

data ConventionSleeping = Gym | Camping | SelfOrganized deriving Show
data Hostel = Hostel deriving Show

participantName :: Participant' status -> Name
participantName p =
    case p of
        (FrisbeeParticipant _ pI _ _) -> name pI
        (JugglingParticipant _ pI _ _) -> name pI

data Participant' status
    = FrisbeeParticipant (MaybePersisted status Id) PersonalInformation Ticket (Either Hostel ConventionSleeping)
    | JugglingParticipant (MaybePersisted status Id) PersonalInformation Ticket ConventionSleeping

type NewParticipant = Participant' 'New
type ExistingParticipant = Participant' 'Persisted

instance Show NewParticipant where
    show (FrisbeeParticipant id_ pI ticket sleeping) = "Frisbee " ++ show id_ ++ show pI ++ show sleeping
    show (JugglingParticipant id_ pI ticket sleeping) = "Juggler " ++ show id_ ++ show pI ++ show sleeping

instance Show ExistingParticipant where
    show (FrisbeeParticipant id_ pI ticket sleeping) = "Frisbee " ++ show id_ ++ show pI ++ show sleeping
    show (JugglingParticipant id_ pI ticket sleeping) = "Juggler " ++ show id_ ++ show pI ++ show sleeping


data Stay = LongStay | ShortStay deriving (Show, Eq)
data AgeCategory = Baby | Child | OlderThan12 deriving (Show, Eq)

type Ticket = (AgeCategory, Stay)

defaultTicket :: Ticket
defaultTicket = (OlderThan12, LongStay)

newtype Price = Price Int

instance Show Price where
    show (Price x)
        | x == 0 = "Kostenlos"
        | otherwise = show x ++ "€"

ticketChoices :: [Ticket]
ticketChoices = [(age, stay) | stay <- [LongStay, ShortStay], age <- [OlderThan12, Child, Baby]]

ticketPrice :: Ticket -> Price
ticketPrice (Baby, _) = Price 0
ticketPrice (Child, ShortStay) = Price 15
ticketPrice (Child, LongStay) = Price 20
ticketPrice (OlderThan12, ShortStay) = Price 30
ticketPrice (OlderThan12, LongStay) = Price 39
