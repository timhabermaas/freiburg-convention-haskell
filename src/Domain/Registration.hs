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
import qualified Data.List.NonEmpty as NE

data Registration' persistedStatus
  = Registration
  { id :: MaybePersisted persistedStatus Id
  , email :: T.Text
  , participants :: NE.NonEmpty (P.Participant' persistedStatus)
  , comment :: Maybe T.Text
  , paymentCode :: MaybePersisted persistedStatus PaymentCode
  , registeredAt :: MaybePersisted persistedStatus UTCTime
  }

type NewRegistration = Registration' 'New
type ExistingRegistration = Registration' 'Persisted
deriving instance Show NewRegistration
