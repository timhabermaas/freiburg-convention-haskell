{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Domain.SharedTypes
  ( Id(..)
  , RegisteredAt(..)
  , Name(..)
  , Birthday(..)
  , MailAddress(MailAddress)
  , mkMailAddress
  , MaybePersisted()
  , PersistedStatus(..)
  , nameEmpty
  , PaymentCode(..)
  ) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)

newtype Id = Id Int deriving (Show, Eq)
newtype RegisteredAt = RegisteredAt UTCTime deriving Show
newtype Birthday = Birthday Day deriving Show
newtype Name = Name T.Text deriving Show

newtype MailAddress = MkMailAddress T.Text deriving Show

pattern MailAddress :: T.Text -> MailAddress
pattern MailAddress content = MkMailAddress content
{-# COMPLETE MailAddress :: MailAddress #-}

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

nameEmpty :: Name -> Bool
nameEmpty (Name t) = T.null $ T.strip t

newtype PaymentCode = PaymentCode T.Text deriving (Eq, Show)
