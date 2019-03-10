{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Domain.SharedTypes
  ( Id(..)
  , RegisteredAt(..)
  , Name(..)
  , Birthday(..)
  , MailAddress(MailAddress)
  , mkMailAddress
  ) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)

newtype Id = Id Int
newtype RegisteredAt = RegisteredAt UTCTime
newtype Birthday = Birthday Day
newtype Name = Name T.Text

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
