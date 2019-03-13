{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Registration
  ( Registration'(..)
  , NewRegistration
  , ExistingRegistration
  ) where

import Domain.SharedTypes
import qualified Domain.Participant as P

import qualified Data.Text as T

data Registration' persistedStatus
  = Registration
  { id :: () --Id
  , email :: T.Text
  , participants :: [P.Participant' persistedStatus]
  }

type NewRegistration = Registration' 'New
type ExistingRegistration = Registration' 'Persisted
deriving instance Show NewRegistration
