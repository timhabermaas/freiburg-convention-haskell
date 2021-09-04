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
  , Address(..)
  , emptyAddress
  , addressIsEmpty
  , formatAddress
  , participantName
  , participantBirthday
  , participantTicket
  , participantAccommodation
  , participantAddress
  , Ticket(..)
  , ticketLabel
  , defaultTicket
  , Stay(..)
  , AgeCategory(..)
  , selectableTickets
  , ticketFromId
  , ageLabel
  , stayLabel
  , gymSleepCount
  , campingSleepCount
  ) where

import Domain.SharedTypes
import Prelude hiding (id)
import qualified Data.Text as T

data PersonalInformation = PersonalInformation
  { name :: Name
  , birthday :: Birthday
  } deriving Show

data Address = Address
  { addressStreet :: T.Text
  , addressPostalCode :: T.Text
  , addressCity :: T.Text
  , addressCountry :: T.Text
  } deriving (Show, Eq)

emptyAddress :: Address
emptyAddress = Address "" "" "" ""

addressIsEmpty :: Address -> Bool
addressIsEmpty (Address street postalCode city country) = street == "" || postalCode == "" || city == "" || country == ""

formatAddress :: Address -> T.Text
formatAddress Address{..}
  = T.intercalate ", " [addressStreet, addressPostalCode <> " " <> addressCity, "(" <> addressCountry <> ")"]

data Accommodation = Gym | Camping | SelfOrganized deriving (Show, Eq)

gymSleepCount :: [Accommodation] -> Int
gymSleepCount = length . filter (== Gym)

campingSleepCount :: [Accommodation] -> Int
campingSleepCount = length . filter (== Camping)

participantName :: Participant' status -> Name
participantName (Participant' _ pI _ _ _) = name pI

participantBirthday :: Participant' status -> Birthday
participantBirthday (Participant' _ pI _ _ _) = birthday pI

participantTicket :: Participant' status -> Ticket
participantTicket (Participant' _ _ _ t _) = t

participantAccommodation :: Participant' status -> Accommodation
participantAccommodation (Participant' _ _ _ _ acc) = acc

participantAddress :: Participant' status -> Address
participantAddress (Participant' _ _ adr _ _) = adr

data Participant' status
    = Participant'
    { pId :: (MaybePersisted status Id)
    , pPersonalInformation :: PersonalInformation
    , pAddress :: Address
    , pTicket :: Ticket
    , pAccommodation :: Accommodation
    }

type NewParticipant = Participant' 'New
type ExistingParticipant = Participant' 'Persisted

instance Show NewParticipant where
    show (Participant' id_ pI address ticket details) = show id_ ++ show pI ++ show address ++ show details ++ show ticket

instance Show ExistingParticipant where
    show (Participant' id_ pI address ticket details) = show id_ ++ show pI ++ show address ++ show details ++ show ticket


data Stay
  -- | represents staying from Thursday to Sunday
  = LongStay
  -- | represents staying from Friday to Sunday
  | ShortStay
  deriving (Show, Eq)

data AgeCategory
  = Baby
  | Child
  | OlderThan12
  | Supporter
  deriving (Show, Eq)

data Ticket = Ticket { id :: Id, ageCategory :: AgeCategory, stay :: Stay, price :: Price, selectable :: Bool } deriving Show

ticketLabel :: Ticket -> T.Text
ticketLabel Ticket{..} = stayLabel stay <> ", " <> ageLabel ageCategory <> ": " <> priceLabel price
  where
    priceLabel price_ = T.pack $ show price_

ageLabel :: AgeCategory -> T.Text
ageLabel Baby = "0–3 Jahre"
ageLabel Child = "4–12 Jahre"
ageLabel OlderThan12 = ">12 Jahre"
ageLabel Supporter = ">12 Jahre (Supporter)"

stayLabel :: Stay -> T.Text
stayLabel LongStay = "Do.–So."
stayLabel ShortStay = "Fr.–So."

instance Eq Ticket where
    t1 == t2 = id t1 == id t2

defaultTicket :: Ticket
defaultTicket = head allTickets

selectableTickets :: [Ticket]
selectableTickets = filter isSelectable allTickets
  where
    isSelectable (Ticket _ _ _ _ s) = s

-- NOTE: Never remove or change any entries in the following list. If you do
-- so, a participant might've ordered a ticket for € 14, but the invoice shows
-- € 18 at a later time leading to us rejecting their money transfer. Removing
-- tickets is even worse since then we won't be able to associate participants
-- with tickets at all and either crash or display no ticket.
-- If you e.g. want to change the price of a ticket, add a new ticket (with a
-- distinct ID) and make the ticket you want to "update" unselectable. The last
-- boolean flag represents whether a ticket can be selected, so change this
-- from `True` to `False`.
allTickets :: [Ticket]
allTickets =
    [ Ticket (Id 13) OlderThan12 LongStay (Price 45) False
    , Ticket (Id 14) Child LongStay (Price 23) False
    , Ticket (Id 15) Baby LongStay (Price 0) False
    , Ticket (Id 16) OlderThan12 ShortStay (Price 20) True
    , Ticket (Id 19) Supporter ShortStay (Price 30) True
    , Ticket (Id 17) Child ShortStay (Price 10) True
    , Ticket (Id 18) Baby ShortStay (Price 0) True
    ]

ticketFromId :: Id -> Ticket
ticketFromId id' = head $ filter (\(Ticket id'' _ _ _ _) -> id'' == id') allTickets
