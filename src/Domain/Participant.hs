module Domain.Participant
  ( ConventionSleeping(..)
  , Hostel(..)
  , Participant(..)
  , PersonalInformation(..)
  ) where

import Domain.SharedTypes

data PersonalInformation = PersonalInformation
  { name :: Name
  , birthday :: Birthday
  }

data ConventionSleeping = Gym | Camping | SelfOrganized
data Hostel = Hostel

data Participant
  = FrisbeeParticipant Id PersonalInformation (Either Hostel ConventionSleeping)
  | JugglingParticipant Id PersonalInformation ConventionSleeping
