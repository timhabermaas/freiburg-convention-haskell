{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Form
    ( newRegisterForm
    , Participant(..)
    , BotCheckResult(..)
    ) where

import Types
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DT
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Time.Calendar (Day, fromGregorianValid, fromGregorian)
import qualified Domain.Registration as Domain
import qualified Domain.Participant as Domain
import qualified Domain.SharedTypes as SharedTypes
import Prelude hiding (id)

data BotCheckResult a = IsBot | IsHuman a deriving Show

participantIsEmpty :: Domain.NewParticipant -> Bool
participantIsEmpty = SharedTypes.nameEmpty . Domain.participantName

checkForBot :: Monad m => DF.Form T.Text m a -> DF.Form T.Text m (BotCheckResult a)
checkForBot innerForm = (\t -> if T.null t then IsHuman else const IsBot) <$> "botField" DF..: DF.text Nothing <*> innerForm

newRegisterForm :: Monad m => (GymSleepingLimitReached, CampingSleepingLimitReached) -> DF.Form T.Text m (BotCheckResult Domain.NewRegistration)
newRegisterForm _ = checkForBot $
    Domain.Registration <$> pure ()
                        <*> "email" DF..: mustBePresent (DF.text Nothing)
                        <*> "participants" DF..: mustContainAtLeastOne (fmap (filter (not . participantIsEmpty)) $ DF.listOf participantForm (Just $ replicate 5 defaultParticipant))
                        <*> "comment" DF..: optionalText
                        <*> pure ()
                        <*> pure ()
  where
    defaultParticipant :: Domain.NewParticipant
    defaultParticipant =
        Domain.Participant' () (Domain.PersonalInformation (SharedTypes.Name "") (SharedTypes.Birthday $ fromGregorian 2000 10 10)) Domain.defaultTicket (Domain.JugglerDetail Domain.Gym)

participantForm :: Monad m => DF.Formlet T.Text m Domain.NewParticipant
participantForm _def =
    buildParticipant <$> "name" DF..: DF.text Nothing
                     <*> "birthday" DF..: birthdayFields
                     <*> "ticket" DF..: ticketForm
                     <*> "accommodation" DF..: sleepingForm
  where
    buildParticipant :: T.Text -> Day -> Domain.Ticket -> Domain.ConventionSleeping -> Domain.NewParticipant
    buildParticipant name birthday ticket sleeping = Domain.Participant' () (Domain.PersonalInformation (SharedTypes.Name name) (SharedTypes.Birthday birthday)) ticket (Domain.JugglerDetail sleeping)

sleepingForm :: Monad m => DF.Form T.Text m Domain.ConventionSleeping
sleepingForm = DF.choice allChoices $ Just Domain.Gym
  where
    allChoices :: [(Domain.ConventionSleeping, T.Text)]
    allChoices =
        [ (Domain.Gym, "Ich schlafe in der Schlafhalle")
        , (Domain.Camping, "Ich schlafe im Zelt neben der Halle")
        , (Domain.SelfOrganized, "Ich sorge für meine eigene Übernachtung")
        ]

ticketForm :: Monad m => DF.Form T.Text m Domain.Ticket
ticketForm = DF.choice ticketChoicesWithLabel (Just Domain.defaultTicket)
  where
    -- TODO: identifier should probably not be the entire Domain.Ticket, but only the id
    ticketChoicesWithLabel :: [(Domain.Ticket, T.Text)]
    ticketChoicesWithLabel = zip Domain.ticketChoices $ ticketChoiceLabel <$> Domain.ticketChoices
    ticketChoiceLabel Domain.Ticket{..} = Domain.stayLabel stay <> ", " <> Domain.ageLabel ageCategory <> ": " <> priceLabel price
    priceLabel price = T.pack $ show price

-- TODO: This might benefit from using Selective Functors. We want to make a decision based on the BotStatus
{-
registerForm :: (Monad m) => (GymSleepingLimitReached, CampingSleepingLimitReached) -> DF.Form T.Text m (BotStatus, Participant)
registerForm isOverLimit =
    (,) <$> botField
        <*> participant
  where
    -- Preventing bot form submissions by checking for a form field not being filled out.
    botField = (\t -> if T.null t then IsHuman else IsBot) <$> "botField" DF..: DF.text Nothing
    participant = Participant <$> "name" DF..: mustBePresent (DF.text Nothing)
                              <*> "birthday" DF..: birthdayFields
                              <*> pure ""
                              <*> pure ""
                              <*> pure ""
                              <*> optionalSleepover
                              <*> pure ""
                              <*> "comment" DF..: optionalText
                              <*> "email" DF..: optionalText

    optionalText =
        (\t -> if T.null t then Nothing else Just t) <$> DF.text Nothing
    optionalSleepover =
        case isOverLimit of
            (GymSleepingLimitReached, CampingSleepingLimitReached) -> pure CouldntSelect
            (EnoughGymSleepingSpots, CampingSleepingLimitReached) -> "sleepover" DF..: DF.choice (filter (\(s, _) -> s /= Camping) allChoices) (Just GymSleeping)
            (GymSleepingLimitReached, EnoughTentSpots) -> "sleepover" DF..: DF.choice (filter (\(s, _) -> s /= GymSleeping) allChoices) (Just Camping)
            (EnoughGymSleepingSpots, EnoughTentSpots) -> "sleepover" DF..: DF.choice allChoices (Just GymSleeping)

    allChoices =
        [ (GymSleeping, "Ich schlafe im Klassenzimmer.")
        , (Camping, "Ich schlafe im Zelt auf dem Schulgelände.")
        , (NoNights, "Ich sorge für meine eigene Übernachtung.")
        ]
-}

birthdayFields :: Monad m => DF.Form T.Text m Day
birthdayFields =
    DF.validate (maybe (DT.Error "kein gültiges Datum") DT.Success)
  $ fromGregorianValid <$> "year" DF..: DF.choice years (Just 1990)
                       <*> "month" DF..: DF.choice months (Just 1)
                       <*> "day" DF..: DF.choice days (Just 1)
  where
    years :: [(Integer, T.Text)]
    years = fmap (\y -> (y, T.pack $ show y)) [1850..2019]

    months :: [(Int, T.Text)]
    months =
        [ (1, "Januar")
        , (2, "Februar")
        , (3, "März")
        , (4, "April")
        , (5, "Mai")
        , (6, "Juni")
        , (7, "Juli")
        , (8, "August")
        , (9, "September")
        , (10, "Oktober")
        , (11, "November")
        , (12, "Dezember")
        ]

    days :: [(Int, T.Text)]
    days = fmap (\y -> (y, T.pack $ show y)) [1..31]


mustBePresent :: (Monad m) => DF.Form T.Text m T.Text -> DF.Form T.Text m T.Text
mustBePresent = DF.check "muss ausgefüllt werden" notEmpty
  where
    notEmpty = not . T.null . T.strip

optionalText :: Monad m => DF.Form T.Text m (Maybe T.Text)
optionalText =
    (\t -> if T.null (T.strip t) then Nothing else Just (T.strip t)) <$> DF.text Nothing

mustContainAtLeastOne :: Monad m => DF.Form T.Text m [a] -> DF.Form T.Text m (NE.NonEmpty a)
mustContainAtLeastOne = DF.validate nonEmptyOrList
  where
    nonEmptyOrList list =
        if null list
            then DT.Error "Mindestens ein Teilnehmer muss angegeben werden."
            else DT.Success $ NE.fromList list
