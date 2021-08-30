{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Domain.Participant
  ( Accommodation(..)
  , Participant'(..)
  , ExistingParticipant
  , NewParticipant
  , PersonalInformation(..)
  , participantName
  , participantBirthday
  , participantTicket
  , participantAccommodation
  , Ticket(..)
  , ticketLabel
  , defaultTicket
  , Stay(..)
  , AgeCategory(..)
  , jugglerTicketChoices
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

data Accommodation = Gym | Camping | SelfOrganized deriving (Show, Eq)

participantName :: Participant' status -> Name
participantName (Participant' _ pI _ _) = name pI

participantBirthday :: Participant' status -> Birthday
participantBirthday (Participant' _ pI _ _) = birthday pI

participantTicket :: Participant' status -> Ticket
participantTicket (Participant' _ _ t _) = t

participantAccommodation :: Participant' status -> Accommodation
participantAccommodation (Participant' _ _ _ acc) = acc

data Participant' status
    = Participant' (MaybePersisted status Id) PersonalInformation Ticket Accommodation

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

jugglerTicketChoices :: [Ticket]
jugglerTicketChoices =
    [ Ticket (Id 13) OlderThan12 LongStay (Price 45)
    , Ticket (Id 14) Child LongStay (Price 23)
    , Ticket (Id 15) Baby LongStay (Price 0)
    , Ticket (Id 16) OlderThan12 ShortStay (Price 36)
    , Ticket (Id 17) Child ShortStay (Price 18)
    , Ticket (Id 18) Baby ShortStay (Price 0)
    ]

ticketFromId :: Id -> Ticket
ticketFromId id' = head $ filter (\(Ticket id'' _ _ _) -> id'' == id') jugglerTicketChoices
