{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.SharedTypes
  ( Id(..)
  , RegisteredAt(..)
  , Name(..)
  , Birthday(..)
  , MailAddress(MailAddress)
  , fromMailAddress
  , mkMailAddress
  , MaybePersisted()
  , PersistedStatus(..)
  , nameEmpty
  , PaymentCode(..)
  , Price(..)
  , City(..)
  , Country(..)
  , PhoneNumber(..)
  , PaidStatus(..)
  , paidStatusFromMaybeTime
  ) where

import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import GHC.Generics

newtype Id = Id Int deriving (Show, Eq)
newtype RegisteredAt = RegisteredAt UTCTime deriving Show
newtype Birthday = Birthday Day deriving Show
newtype Name = Name T.Text deriving (Show, Eq)

instance Ord Name where
    compare (Name x) (Name y) = compare (T.toUpper x) (T.toUpper y)

newtype City = City T.Text deriving (Show, Generic)
newtype Country = Country T.Text deriving (Show, Generic)
newtype PhoneNumber = PhoneNumber T.Text deriving (Show, Generic)

instance ToJSON Country
instance FromJSON Country
instance ToJSON City
instance FromJSON City
instance ToJSON PhoneNumber
instance FromJSON PhoneNumber

newtype MailAddress = MkMailAddress T.Text deriving Show

pattern MailAddress :: T.Text -> MailAddress
pattern MailAddress content = MkMailAddress content
{-# COMPLETE MailAddress :: MailAddress #-}

fromMailAddress :: MailAddress -> T.Text
fromMailAddress (MkMailAddress addr) = addr

mkMailAddress :: T.Text -> Maybe MailAddress
mkMailAddress text =
    if T.null $ T.filter (== '@') text then
        Nothing
    else
        Just $ MailAddress text

data PersistedStatus = Persisted | New

type family MaybePersisted (status :: PersistedStatus) a where
    MaybePersisted 'Persisted a = a
    MaybePersisted 'New a = ()

-- TODO: Should be a smart constructor
nameEmpty :: Name -> Bool
nameEmpty (Name t) = T.null $ T.strip t

newtype PaymentCode = PaymentCode T.Text deriving (Eq, Show)
newtype Price = Price Int deriving (Eq, Num)

instance Show Price where
    show (Price x) = show x <> "€"

data PaidStatus = Paid | NotPaid deriving (Show, Eq)

paidStatusFromMaybeTime :: Maybe UTCTime -> PaidStatus
paidStatusFromMaybeTime (Just _) = Paid
paidStatusFromMaybeTime Nothing = NotPaid
