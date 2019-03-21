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
import Data.Time.Clock (UTCTime)

data Registration' persistedStatus
  = Registration
  { id :: MaybePersisted persistedStatus Id
  , email :: T.Text
  , participants :: [P.Participant' persistedStatus] -- TODO: Use non-empty list
  , comment :: Maybe T.Text
  , paymentCode :: MaybePersisted persistedStatus PaymentCode
  , registeredAt :: MaybePersisted persistedStatus UTCTime
  }

type NewRegistration = Registration' 'New
type ExistingRegistration = Registration' 'Persisted
deriving instance Show NewRegistration
