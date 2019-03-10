module Domain.Registration
  ( Registration(..)
  ) where

import Domain.SharedTypes
import qualified Domain.Participant as P

import qualified Data.Text as T

data Registration
  = Registration
  { id :: Id
  , email :: T.Text
  , participants :: [P.Participant]
  }
