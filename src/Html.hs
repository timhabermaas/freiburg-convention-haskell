{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Html
  ( registerPage
  , successPage
  , registrationListPage'
  , participationPrintPage
  , participationListPage
  , Html
  )
where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!)
                                                , (!?)
                                                )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Text.Digestive.Blaze.Html5    as DH
import qualified Text.Digestive.View           as DV
import qualified Data.Text                     as T
import           Data.Monoid                    ( (<>) )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Data.Time.LocalTime            ( utcToZonedTime
                                                , hoursToTimeZone
                                                , ZonedTime
                                                )
import           Data.Maybe                     ( fromMaybe
                                                )
import           Prelude                 hiding ( id )
import           Data.Coerce                    ( coerce )
import           Control.Monad                  ( guard )

import           Types
import           Util
import qualified Domain.Registration           as R
import qualified Domain.Participant            as P
import qualified Domain.SharedTypes            as DT

type Html = H.Html

instance H.ToMarkup DT.PaymentCode where
  toMarkup (DT.PaymentCode x) = H.toMarkup x

layout :: H.Html -> H.Html
layout inner = do
  H.docType
  H.html ! A.lang "de" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content
        "width=device-width, initial-scale=1, shrink-to-fit=no"
      H.title "Freiburger Convention 2021"
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href
            "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href
        "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css"
    H.body $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "mb-3" $ mempty
        inner

instance H.ToMarkup DT.Name where
  toMarkup (DT.Name x) = H.toMarkup x


filteredParticipants
  :: [P.ExistingParticipant] -> P.Accommodation -> P.Stay -> Int
filteredParticipants participants accommodation stay = length $ do
  p@(P.Participant' _ _ _ (P.Ticket _ _ pStay _ _) _) <- participants
  guard $ pStay == stay
  guard $ P.participantAccommodation p == accommodation
  pure p

filteredParticipants' :: [P.ExistingParticipant] -> P.Accommodation -> Int
filteredParticipants' participants accommodation = length $ do
  p <- participants
  guard $ P.participantAccommodation p == accommodation
  pure p


participationListPage
  :: [(P.ExistingParticipant, R.ExistingRegistration)] -> ParticipantLimits -> H.Html
participationListPage participants' ParticipantLimits{..} = layout $ do
  let participants = fmap fst participants'
  row $ do
    colMd 12 $ do
      H.h1 "Teilnehmer*innen"
  rowWithSpace $ do
    colMd 12 $ do
      H.ul ! A.class_ "nav nav-pills" $ do
        H.li ! A.class_ "nav-item" $ do
          H.a ! A.class_ "nav-link" ! A.href "/admin" $ "Anmeldungen"
        H.li ! A.class_ "nav-item" $ do
          H.a
            ! A.class_ "nav-link active"
            ! A.href "/admin/participants"
            $ "Teilnehmer*innen"
  row $ do
    colMd 12 $ do
      H.table ! A.class_ "table" $ do
        H.thead $ do
          H.th mempty
          H.th ! A.class_ "text-right" $ "Do–So"
          H.th ! A.class_ "text-right" $ "Fr–So"
          H.th ! A.class_ "text-right" $ "Summe"
        H.tbody $ do
          H.tr $ do
            H.th $ "Schlafhalle"
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.Gym
              P.LongStay
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.Gym
              P.ShortStay
            H.td ! A.class_ "text-right" $ do
                H.strong $ H.toHtml $ filteredParticipants' participants P.Gym
                H.small $ H.toHtml $ " (max " <> show @Int (coerce gymSleeping) <> ")"
          H.tr $ do
            H.th $ "Camping"
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.Camping
              P.LongStay
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.Camping
              P.ShortStay
            H.td ! A.class_ "text-right" $ do
                H.strong $ H.toHtml $ filteredParticipants' participants P.Camping
                H.small $ H.toHtml $ " (max " <> show @Int (coerce campingSleeping) <> ")"
          H.tr $ do
            H.th $ "Woanders"
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.SelfOrganized
              P.LongStay
            H.td ! A.class_ "text-right" $ H.toHtml $ filteredParticipants
              participants
              P.SelfOrganized
              P.ShortStay
            H.td
              ! A.class_ "text-right"
              $ H.strong
              $ H.toHtml
              $ filteredParticipants' participants P.SelfOrganized
          H.tr $ do
            H.td mempty
            H.td mempty
            H.td mempty
            H.td ! A.class_ "text-right" $ do
                H.strong $ H.toHtml $ length participants
                H.small $ H.toHtml $ " (max " <> show @Int (coerce overallLimit) <> ")"
  rowWithSpace $ do
    colMd 12 $ do
      H.a ! A.href "/admin/participants/print" $ "Druckansicht"
  row $ do
    colMd 12 $ do
      H.table ! A.class_ "table" $ do
        H.thead $ do
          H.tr $ do
            H.th "Name"
            H.th "Geburtsdatum"
            H.th "Postadresse"
            H.th "Ticket"
            H.th "Schlafgelegenheit"
            H.th "Bezahlt?"
            H.th "Mitteilung"
        H.tbody $ mapM_ participantRow participants'
 where
  participantRow :: (P.ExistingParticipant, R.ExistingRegistration) -> H.Html
  participantRow (p, R.Registration {..}) = do
    H.tr $ do
      H.td $ H.toHtml $ P.participantName p
      H.td $ H.toHtml $ formatDay $ coerce $ P.participantBirthday p
      H.td $ H.toHtml $ P.formatAddress $ P.participantAddress p
      H.td $ H.toHtml $ P.ticketLabel $ P.participantTicket p
      H.td $ H.toHtml $ sleepOverToGerman $ P.participantAccommodation p
      H.td $ H.toHtml $ paidToText paidStatus
      H.td $ H.toHtml $ fromMaybe "" comment

registrationListPage'
  :: [R.ExistingRegistration]
  -> H.Html
registrationListPage' registrations
  = layout $ do
    row $ do
      colMd 12 $ do
        H.h1 "Anmeldungen"
    rowWithSpace $ do
      colMd 12 $ do
        H.ul ! A.class_ "nav nav-pills" $ do
          H.li ! A.class_ "nav-item" $ do
            H.a ! A.class_ "nav-link active" ! A.href "/admin" $ "Anmeldungen"
          H.li ! A.class_ "nav-item" $ do
            H.a
              ! A.class_ "nav-link"
              ! A.href "/admin/participants"
              $ "Teilnehmer*innen"
    row $ do
      colMd 12 $ do
        H.div ! A.class_ "alert alert-primary" $ do
          H.ul $ do
            H.li $ do
              H.strong $ H.toHtml $ length registrations
              " Anmeldungen"
    rowWithSpace $ do
      colMd 12 $ do
        H.a ! A.href "/admin/registrations.csv" $ "CSV-Export"

    row $ do
      colMd 12 $ do
        H.table ! A.class_ "table" $ do
          H.thead $ do
            H.tr $ do
              H.th "E-Mail"
              H.th "Anzahl Teilnehmer*innen"
              H.th "Mitteilung"
              H.th "Angemeldet am"
              H.th "Verwendungszweck"
              H.th "Summe Tickets"
              H.th "Bezahlt?"
              H.th "Aktionen"
          H.tbody $ mapM_ registrationRow registrations
    {-row $ do
        colMd 3 $ do
            H.a ! A.href "/registrations.csv" $ "Download als .csv"
        colMd 3 $ do
            H.a ! A.href "/registrations/print" $ "Print stuff"
        -}






 where
  registrationRow :: R.ExistingRegistration -> H.Html
  registrationRow reg@R.Registration {..} = H.tr $ do
    H.td $ H.toHtml email
    H.td $ H.toHtml $ length participants
    H.td $ H.toHtml $ fromMaybe "" comment
    H.td
      $ H.toHtml
      $ formatTime defaultTimeLocale "%d.%m.%Y %H:%M Uhr"
      $ utcToBerlin registeredAt
    H.td $ H.toHtml $ paymentCode
    H.td $ H.toHtml $ show $ R.priceToPay reg
    H.td $ if paidStatus == DT.Paid then "✓" else "✘"
    H.td $ do
      row $ do
        colMd 6 $ do
          H.form
            ! A.action
                (H.toValue $ "/registrations/" <> idToText id <> "/delete")
            ! A.method "post"
            $ do
                H.input
                  ! A.onclick
                      (  H.toValue
                      $  "return confirm('Willst du wirklich ' + '"
                      <> email
                      <> "' + ' ausladen?');"
                      )
                  ! A.class_ "btn btn-danger"
                  ! A.type_ "submit"
                  ! A.name "delete"
                  ! A.value "Löschen"
        colMd 6 $ do
          case paidStatus of
            DT.Paid -> mempty
            DT.NotPaid ->
              H.form
                ! A.action
                    (H.toValue $ "/registrations/" <> idToText id <> "/pay")
                ! A.method "post"
                $ do
                    H.input
                      ! A.class_ "btn btn-primary"
                      ! A.type_ "submit"
                      ! A.value "Bezahlt"
  idToText (DT.Id i) = T.pack $ show i

utcToBerlin :: UTCTime -> ZonedTime
utcToBerlin = utcToZonedTime (hoursToTimeZone 2)

successPage :: H.Html
successPage = layout $ do
  row $ do
    colMd 12 $ do
      H.h1 "Danke für deine Anmeldung!" ! A.class_ "text-center"
      H.p ! A.class_ "text-center" $ do
        "Du solltest in Kürze eine E-Mail von uns erhalten. Falls nicht, melde dich bitte unter "
        mailLink "orga@jonglieren-in-freiburg.de" "orga@jonglieren-in-freiburg.de"
        "."

modifiedView :: DV.View T.Text -> DV.View H.Html
modifiedView = fmap H.toHtml

alert :: T.Text -> H.Html
alert text = do
  H.div ! A.class_ "alert alert-danger" $ H.toHtml text

info :: T.Text -> H.Html
info text = do
  H.div ! A.class_ "alert alert-primary" $ H.toHtml text

noSleepingMessage
  :: LimitReached -> H.Html
noSleepingMessage NoLimitReached = mempty
noSleepingMessage CampingLimitReached = mempty
noSleepingMessage SleepingAtSideLimitReached =
  info
    "Leider sind schon alle Schlafplätze belegt. Du kannst dich aber trotzdem anmelden und vorbei kommen, solange du dir einen eigenen Schlafplatz organisierst."
noSleepingMessage GymLimitReached =
  alert
    "Leider sind schon alle Schlafplätze in der Schlafhalle belegt. Du kannst dich aber trotzdem anmelden und entweder im Zelt schlafen oder dir einen eigenen Schlafplatz organisieren."
noSleepingMessage OverallLimitReached =
  alert
    "Leider sind wir schon ausgebucht und können keine weiteren Teilnehmer*innen mehr aufnehmen. Wir bitten euch nicht spontan vorbeizukommen. Auf Grund der Corona-Verordnungen können wir keine weiteren Teilnehmer*innern zulassen und müssten euch wieder heimschicken."

participantForm :: DV.View T.Text -> Int -> H.Html
participantForm view currentIndex = do
  H.div ! A.class_ "participant" $ do
    row $ do
      colMd 12 $ do
        H.br
        H.h4 $ H.toHtml $ show currentIndex ++ ". Teilnehmer*in"
        H.div ! A.class_ "form-group" $ do
          label "Name" "Full Name" "name" view
          DH.inputText "name" view ! A.class_ "form-control" ! A.autocomplete "name"
          formErrorMessage "name" view
        dateForm "Geburtsdatum" "Birthday" $ DV.subView "birthday" view
        addressForm $ DV.subView "address" view
        row $ do
          colMd 12 $ do
            H.div ! A.class_ "form-group" $ do
              label "Festivalticket" "Ticket" "ticket" view
              bootstrapRadios "ticket" (modifiedView view)
              formErrorMessage "ticket" view
        row $ do
          colMd 12 $ do
            H.div ! A.class_ "form-group" $ do
              label "Unterkunft" "Accommodation" "accommodation" view
              DH.inputSelect "accommodation" (modifiedView view)
                ! A.class_ "form-control"
              formErrorMessage "accommodation" view

jugglingRegisterForm :: DV.View T.Text -> H.Html
jugglingRegisterForm view = do
  H.form ! A.action "/register" ! A.method "post" $ do
    H.div ! A.class_ "form-group" $ do
      label "E-Mail" "Email" "email" view
      DH.inputText "email" view ! A.class_ "form-control" ! A.autocomplete "email"
      formErrorMessage "email" view
    H.div ! A.class_ "form-group d-none" $ do
      label "Name" "Full Name" "botField" view
      DH.inputText "botField" view ! A.class_ "form-control"

    formErrorMessage "participants" view
    let participantViews = DV.listSubViews "participants" view
    H.div ! A.class_ "participants" $ do
      mapM_ (\(v, i) -> participantForm v i)
        $     participantViews
        `zip` [1 ..]
    let indicesList = T.intercalate "," $ T.pack <$> show <$> (\(x, _) -> x) <$> zip [0 :: Int ..] participantViews
    H.input ! A.type_ "hidden" ! A.name "Registration.participants.indices" ! A.value (H.toValue indicesList)

    H.div $ do
      H.a ! A.href "#" ! A.id "link" ! A.data_ (H.toValue $ show $ length participantViews) $ do
        "Weitere Teilnehmer*innen anmelden"
        H.span
          ! A.class_ "text-secondary"
          $ " (Register additional participants)"
    H.br
    H.br

    H.div ! A.class_ "form-group" $ do
      label "Willst du uns noch etwas mitteilen?"
            "Anything you want to tell us?"
            "comment"
            view
      DH.inputTextArea Nothing Nothing "comment" (modifiedView view)
        ! A.class_ "form-control"
      formErrorMessage "comment" view

    H.div ! A.class_ "form-group" $ do
      bootstrapBooleanCheckbox "covidTermsAccepted" "Ich bestätige, dass alle Teilnehmer*innen bei Ankunft entweder geimpft, genesen oder getestet sind (3G) und die entsprechenden Nachweise vorlegen können." (modifiedView view)
      formErrorMessage "covidTermsAccepted" view

    H.div ! A.class_ "form-group" $ do
      H.input ! A.class_ "btn btn-primary" ! A.type_ "submit" ! A.value
        "Anmelden"

    H.script $ do
      let code :: [String] =
            [ "function replaceNumber(name, nextNumber) {"
            , "  return name.replace(/\\.\\d+\\./, '.' + nextNumber + '.');"
            , "}"
            , "function replaceNumberInAttribute(e, attr, nextNumber) {"
            , "  var value = e.getAttribute(attr);"
            , "  e.setAttribute(attr, replaceNumber(value, nextNumber));"
            , "};"
            , "var link = document.getElementById('link');"
            , "link.addEventListener('click', function(e){"
            , "   e.preventDefault();"

            , "   var nextId = parseInt(this.getAttribute('data'), 10);"
            , "   var participants = document.querySelectorAll('.participant');"
            , "   var lastForm = participants[participants.length - 1];"
            , "   var newForm = lastForm.cloneNode(true);"
            , "   newForm.querySelectorAll('input, select').forEach(function(e) { replaceNumberInAttribute(e, 'name', nextId); replaceNumberInAttribute(e, 'id', nextId); });"
            , "   newForm.querySelectorAll('input[type=text]').forEach(function(e) { e.value = ''; });"
            , "   newForm.querySelectorAll('label').forEach(function(e) { replaceNumberInAttribute(e, 'for', nextId) });"
            , "   var headline = newForm.querySelector('h4');"
            , "   headline.innerText = '' + (nextId + 1) + '. Teilnehmer*in';"
            , "   lastForm.parentNode.insertBefore(newForm, null);"
            , "   this.setAttribute('data', nextId + 1);"
            , "   var indicesElement = document.querySelector(\"input[name='Registration.participants.indices']\");"
            , "   indicesElement.value = Array.apply(null, Array(nextId + 1)).map(function (x, i) { return i; }).join(',');"
            , "})"
            , ""
            ]
      H.toHtml $ unlines code

registerPage
  :: DV.View T.Text
  -> LimitReached
  -> H.Html
registerPage view isOverLimit = layout $ do
  H.div ! A.class_ "row mb-4" $ do
    colMd 12 $ do
      H.h1 ! A.class_ "text-center" $ "Anmeldung zur Freiburger Jonglierconvention 2021"
      H.h4 ! A.class_ "text-center" $ H.small ! A.class_ "text-muted" $ "24.09.2021 – 26.09.2021"

  H.div ! A.class_ "row justify-content-center" $ do
    colLg 6 $ do
      noSleepingMessage isOverLimit

      if isOverLimit == OverallLimitReached
        then
          mempty
        else do
          H.br
          jugglingRegisterForm view

addressForm :: DV.View T.Text -> Html
addressForm addressView = do
  H.div ! A.class_ "form-group" $ do
    label "Straße" "Street" "" addressView
    DH.inputText "street" (modifiedView addressView) ! A.class_ "form-control" ! A.autocomplete "street-address"
    formErrorMessage "street" addressView
  H.div ! A.class_ "form-group" $ do
    row $ do
      colMd 4 $ do
        label "PLZ" "Postal Code" "" addressView
        DH.inputText "postalCode" (modifiedView addressView) ! A.class_ "form-control" ! A.autocomplete "postal-code"
        formErrorMessage "postalCode" addressView
      colMd 8 $ do
        label "Stadt" "City" "" addressView
        DH.inputText "city" (modifiedView addressView) ! A.class_ "form-control" ! A.autocomplete "address-level2"
        formErrorMessage "city" addressView
  H.div ! A.class_ "form-group" $ do
    label "Land" "Country" "" addressView
    DH.inputText "country" (modifiedView addressView) ! A.class_ "form-control" ! A.autocomplete "country-name"
    formErrorMessage "country" addressView

dateForm :: T.Text -> T.Text -> DV.View T.Text -> Html
dateForm labelText englishLabelText dateView = do
  H.div ! A.class_ "form-group" $ do
    label labelText englishLabelText "" dateView
    row $ do
      H.div ! A.class_ "col-sm-3" $ do
        DH.inputSelect "day" (modifiedView dateView) ! A.class_ "form-control"
      H.div ! A.class_ "col-sm-5 mt-2 mt-sm-0" $ do
        DH.inputSelect "month" (modifiedView dateView) ! A.class_ "form-control"
      H.div ! A.class_ "col-sm-4 mt-2 mt-sm-0" $ do
        DH.inputSelect "year" (modifiedView dateView) ! A.class_ "form-control"
    row $ do
      colMd 12 $ do
        formErrorMessage "" dateView


bootstrapBooleanCheckbox :: T.Text -> T.Text -> DV.View Html -> Html
bootstrapBooleanCheckbox ref text view =
  let checked = DV.fieldInputBool ref view
      ref' :: T.Text
      ref' = DV.absoluteRef ref view
      cssId = H.toValue ref'
  in
    H.div ! A.class_ "form-check" $ do
      H.input
        !  A.id cssId
        !  A.class_ "form-check-input"
        !  A.type_ "checkbox"
        !  A.name (H.toValue ref')
        !? (checked, A.checked "checked")
      H.label ! A.class_ "form-check-label" ! A.for cssId $ H.toHtml $ text


bootstrapCheckboxes :: T.Text -> DV.View Html -> Html
bootstrapCheckboxes ref view =
  let options = DV.fieldInputChoice ref view
      ref' :: T.Text
      ref' = DV.absoluteRef ref view
      checkbox (i, c, selected) = do
        let cssId = H.toValue $ ref' <> i
        H.div ! A.class_ "form-check" $ do
          H.input
            !  A.id cssId
            !  A.class_ "form-check-input"
            !  A.type_ "checkbox"
            !  A.name (H.toValue ref')
            !  A.value (H.toValue i)
            !? (selected, A.checked "checked")
          H.label ! A.class_ "form-check-label" ! A.for cssId $ c
  in  mapM_ checkbox options

checkboxesWithOther :: T.Text -> T.Text -> DV.View T.Text -> Html
checkboxesWithOther labelText englishLabelText view = do
  let choices = DV.fieldInputChoice
  H.div ! A.class_ "form-group" $ do
    label labelText englishLabelText "choice" view
    bootstrapCheckboxes "choice" (modifiedView view)
    DH.inputText "text" (modifiedView view) ! A.class_ "form-control"
    formErrorMessage "choice" view

formErrorMessage :: T.Text -> DV.View T.Text -> Html
formErrorMessage ref view = case DV.errors ref view of
  [] -> mempty
  es -> H.small ! A.class_ "text-danger" $ H.toHtml $ T.intercalate " " es

mailLink :: T.Text -> T.Text -> Html
mailLink text email =
  H.a ! A.href (H.toValue $ "mailto:" <> email) $ H.toHtml text

label :: T.Text -> T.Text -> T.Text -> DV.View a -> Html
label text englishText name view =
  let ref = H.toValue $ DV.absoluteRef name view
  in  H.label ! A.for ref $ do
        H.toHtml text
        if T.null englishText
          then mempty
          else do
            " "
            H.span
              !  A.class_ "text-secondary"
              $  "("
              <> H.toHtml englishText
              <> ")"

bootstrapRadios :: T.Text -> DV.View Html -> Html
bootstrapRadios ref view =
  let options = DV.fieldInputChoice ref view
      ref'    = DV.absoluteRef ref view
      radio (i, c, selected) = do
        let cssId = H.toValue $ ref' <> i
        H.div ! A.class_ "form-check" $ do
          H.input
            !  A.id cssId
            !  A.class_ "form-check-input"
            !  A.type_ "radio"
            !  A.name (H.toValue ref')
            !  A.value (H.toValue i)
            !? (selected, A.checked "selected")
          H.label ! A.class_ "form-check-label" ! A.for cssId $ c
  in  mapM_ radio options

participationPrintPage
  :: [(P.ExistingParticipant, R.ExistingRegistration)] -> H.Html
participationPrintPage participants = layout $ do
  row $ do
    colMd 12 $ do
      H.div ! A.class_ "fixed-header" $ do
        H.h3
          ! A.class_ "text-center"
          $ "Anmeldeliste 22. Freiburger Jonglierfestival – 24. September bis 26. September 2021"
  row $ do
    colMd 12 $ do
      H.table ! A.class_ "table table-bordered table-sm" $ do
        H.thead $ do
          H.tr $ do
            H.td ! A.colspan "10" $ do
              H.strong "Haftungsausschluss: "
              "Mit meiner Unterschrift bestätige ich, dass mir bekannt ist, dass auf dem Jonglierfestival Freiburg von den OrganisatorInnen keine Haftung für eventuell auftretende Verletzungen, Diebstähle etc. übernommen werden kann. Dies gilt auch für alle Zwischenfälle während der Anfahrt oder Rückreise. Weiterhin bestätige ich, dass ich ausreichend versichert bin (Haft- und Unfallversicherung), die Hallenordnung und das Hygienekonzept anerkenne und den Anweisungen der OrganisatorInnen Folge leiste."
              H.br
              H.strong "Erklärung zur Bildnutzung: "
              "Mit meiner Unterschrift erkläre ich mich einverstanden, dass Fotos, die während des Festivals von mir gemacht werden, auf der Webseite "
              H.a
                ! A.href "https://www.jonglieren-in-freiburg.de"
                $ "www.jonglieren-in-freiburg.de"
              " veröffentlicht und für Pressezwecke genutzt werden dürfen."
          H.tr $ do
            H.th ""
            H.th "Name"
            H.th "Geburtsdatum"
            H.th "Ticket"
            H.th "Wo?"
            H.th "Bezahlung"
            H.th "Unterschrift"
        H.tbody $ do
          mapM_ participantRow (zip [(1 :: Int) ..] participants)
          mapM_ emptyRow
                [(length participants + 1) .. (length participants + 150)]

 where
  rowWithMinHeight inner = H.tr ! A.style "line-height: 35px" $ inner
  numberColumn n = H.td ! A.class_ "text-right" $ H.toHtml $ show n
  emptyRow n = rowWithMinHeight $ do
    numberColumn n
    H.td mempty
    H.td mempty
    H.td mempty
    H.td mempty
    H.td mempty
    H.td mempty
    H.td mempty
  participantRow (n, (p, R.Registration {..})) = rowWithMinHeight $ do
    numberColumn n
    H.td $ H.toHtml $ P.participantName p
    H.td
      ! A.style "width: 80px"
      $ H.toHtml
      $ formatBirthday
      $ P.participantBirthday p
    H.td $ H.toHtml $ P.ticketLabel $ P.participantTicket p
    H.td
      ! A.class_ "text-center"
      ! A.style "width: 40px"
      $ H.toHtml
      $ sleepOverToGerman
      $ P.participantAccommodation p
    H.td $ H.toHtml $ paidToText paidStatus
    H.td $ mempty

sleepOverToGerman :: P.Accommodation -> T.Text
sleepOverToGerman P.Camping       = "Zelt"
sleepOverToGerman P.SelfOrganized = "—"
sleepOverToGerman P.Gym           = "Halle"



row :: Html -> Html
row inner = H.div ! A.class_ "row" $ inner

rowWithSpace :: Html -> Html
rowWithSpace inner = H.div ! A.class_ "row mb-2" $ inner

colMd :: Int -> Html -> Html
colMd columns inner =
  H.div ! A.class_ (H.toValue $ "col-md-" ++ show columns) $ inner

colLg :: Int -> Html -> Html
colLg columns inner =
  H.div ! A.class_ (H.toValue $ "col-lg-" ++ show columns) $ inner

paidToText :: DT.PaidStatus -> T.Text
paidToText DT.Paid    = "Bezahlt"
paidToText DT.NotPaid = "Nicht bezahlt"
