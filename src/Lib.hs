{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApp
    , app
    , AdminPassword(..)
    , Config(..)
    ) where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze (HTML)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Media ((//), (/:))
import Data.Semigroup ((<>))
import Control.Concurrent (forkIO)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Digestive.Form.Encoding as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.View as DF
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import qualified IO.Db as Db
import qualified Html as Page
import qualified Form
import qualified Data.Maybe as M
import qualified IO.Mailer as Mailer
import Types
import Util
import qualified Domain.Registration as D
import qualified Domain.Participant as P
import qualified Domain.SharedTypes as DT

data CSV

instance Accept CSV where
    contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV BSL.ByteString where
    mimeRender _ val = val

type API
    = Get '[HTML] Page.Html
 :<|> "register" :> ReqBody '[FormUrlEncoded] [(T.Text, T.Text)] :> Post '[HTML] Page.Html
 :<|> "success" :> Get '[HTML] Page.Html
 :<|> "admin" :> BasicAuth "foo-realm" () :> Get '[HTML] Page.Html
 :<|> "registrations.csv" :> BasicAuth "foo-realm" () :> Get '[CSV] BSL.ByteString
 :<|> "registrations" :> BasicAuth "foo-realm" () :> Capture "participantId" ParticipantId :> "delete" :> Post '[HTML] Page.Html
 :<|> "registrations" :> BasicAuth "foo-realm" () :> "print" :> Get '[HTML] Page.Html

newtype AdminPassword = AdminPassword T.Text

data Config = Config
    { configMailerHandle :: Mailer.Handle
    , configDbHandle :: Db.Handle
    , configAdminPassword :: AdminPassword
    , configSleepingLimits :: (GymSleepingLimit, CampingSleepingLimit)
    }

startApp :: String -> Int -> Int -> Int -> AdminPassword -> Maybe String -> IO ()
startApp dbUrl port participationLimit campingLimit pw maybeSendGridApiKey = do
    let mailerConfig = maybe Mailer.PrinterConfig Mailer.SendGridConfig maybeSendGridApiKey
    Mailer.withConfig mailerConfig $ \mailHandle -> do
        Db.withConfig dbUrl $ \db -> do
            Db.migrate db
            let config = Config { configMailerHandle = mailHandle, configDbHandle = db, configAdminPassword = pw, configSleepingLimits = (GymSleepingLimit participationLimit, CampingSleepingLimit campingLimit) }
            run port $ logStdoutDev $ app config

authCheck :: AdminPassword -> BasicAuthCheck ()
authCheck (AdminPassword pw) =
    let check (BasicAuthData username password) =
            if username == "admin" && password == TE.encodeUtf8 pw
            then pure (Authorized ())
            else pure Unauthorized
    in
        BasicAuthCheck check

authServerContext :: AdminPassword -> Context (BasicAuthCheck () ': '[])
authServerContext pw = (authCheck pw) :. EmptyContext

app :: Config -> Application
app Config{..} =
    serveWithContext
        api
        (authServerContext configAdminPassword)
        (server configDbHandle configMailerHandle configSleepingLimits)

api :: Proxy API
api = Proxy

server :: Db.Handle -> Mailer.Handle -> (GymSleepingLimit, CampingSleepingLimit) -> Server API
server db mailerHandle limits =
         registerHandler db limits
    :<|> postRegisterHandler db mailerHandle limits
    :<|> successHandler
    :<|> registrationsHandler db limits
    :<|> registrationsCsvHandler db
    :<|> deleteRegistrationsHandler db
    :<|> printRegistrationsHandler db

isOverLimit :: Db.Handle -> (GymSleepingLimit, CampingSleepingLimit) -> IO (GymSleepingLimitReached, CampingSleepingLimitReached)
isOverLimit handle (GymSleepingLimit gymLimit, CampingSleepingLimit campingLimit) = do
    pure (EnoughGymSleepingSpots, EnoughTentSpots)
    {-
    sleepovers <- liftIO $ fmap Db.dbParticipantSleepovers <$> Db.allRegistrations handle
    let gymLimitReached =
            if gymSleepCount sleepovers >= gymLimit then
                GymSleepingLimitReached
            else
                EnoughGymSleepingSpots
    let campingLimitReached =
            if campingSleepCount sleepovers >= campingLimit then
                CampingSleepingLimitReached
            else
                EnoughTentSpots
    pure (gymLimitReached, campingLimitReached) -}

registerHandler :: Db.Handle -> (GymSleepingLimit, CampingSleepingLimit) -> Handler Page.Html
registerHandler conn limits = do
    overLimit <- liftIO $ isOverLimit conn limits
    view <- DF.getForm "Registration" $ Form.newRegisterForm overLimit
    liftIO $ putStrLn $ show $ DF.debugViewPaths view
    --view <- DF.getForm "Registration" $ Form.registerForm overLimit
    pure $ Page.registerPage view overLimit

registrationsHandler :: Db.Handle -> (GymSleepingLimit, CampingSleepingLimit) -> () -> Handler Page.Html
registrationsHandler conn limits _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    pure $ Page.registrationListPage registrations limits


-- Using newtype wrapper for Participant because the canonical CSV decoder/encoder for the
-- database row isn't exactly what we want.
newtype CsvParticipant = CsvParticipant Db.DbParticipant

-- The IsString instance of ByteString is not using the source encoding (in this case UTF8),
-- but Char8.pack to convert String to ByteString:
-- `f :: ByteString`, `f "ü"` will result in \252.
-- See https://github.com/haskell/bytestring/issues/140
fixEncoding :: T.Text -> BS.ByteString
fixEncoding = TE.encodeUtf8

instance Csv.ToNamedRecord CsvParticipant where
    toNamedRecord (CsvParticipant Db.DbParticipant{..}) =
        Csv.namedRecord
            [ "Name" Csv..= dbParticipantName
            , "Adresse" Csv..= TE.encodeUtf8 address
            , "Land" Csv..= TE.encodeUtf8 dbParticipantCountry
            , fixEncoding "Übernachtung" Csv..= sleeping dbParticipantSleepovers
            , "Anmerkung" Csv..= (TE.encodeUtf8 <$> dbParticipantComment)
            ]
      where
        address = (dbParticipantStreet <> ", " <> dbParticipantPostalCode <> " " <> dbParticipantCity) :: T.Text
        sleeping s = case s of
            NoNights -> "Keine Übernachtung" :: T.Text
            Camping -> "Zelt"
            GymSleeping -> "Klassenzimmer"
            CouldntSelect -> "Keine Auswahl"

registrationsCsvHandler :: Db.Handle -> () -> Handler BSL.ByteString
registrationsCsvHandler conn _ = do
    registrations <- liftIO $ Db.allRegistrations conn
    let headers = fixEncoding <$> V.fromList [ "Name", "Adresse", "Land", "Übernachtung", "Anmerkung" ]
    pure $ Csv.encodeByName headers $ fmap CsvParticipant registrations

postRegisterHandler :: Db.Handle -> Mailer.Handle -> (GymSleepingLimit, CampingSleepingLimit) -> [(T.Text, T.Text)] -> Handler Page.Html
postRegisterHandler conn mailerHandle limits body = do
    overLimit <- liftIO $ isOverLimit conn limits
    r <- DF.postForm "Registration" (Form.newRegisterForm overLimit) $ servantPathEnv body
    case r of
        (view, Nothing) -> do
            liftIO $ print view
            pure $ Page.registerPage view overLimit
        (_, Just botCheckedRegistration) -> do
            case botCheckedRegistration of
                Form.IsBot -> do
                    liftIO $ putStrLn "is bot"
                    redirectTo "/success"
                Form.IsHuman registration -> do
                    liftIO $ putStrLn $ show registration
                    registrationId <- liftIO $ Db.saveRegistration' conn registration
                    registration <- liftIO $ Db.getRegistration conn registrationId
                    liftIO $ forkIO $ Mailer.sendMail mailerHandle $ mailForRegistration registration
                    {-
                    let to = (M.fromJust $ Form.participantEmail registration >>= Domain.mkMailAddress, Form.participantName registration)
                    let email = Mailer.Mail "hallo! :)" "some subject" to
                    liftIO $ Mailer.sendMail mailerHandle email
                    liftIO $ putStrLn $ show email
                    liftIO $ Db.saveRegistration conn registration
                    -}
                    redirectTo "/success"
    --r <- DF.postForm "Registration" (Form.registerForm overLimit) $ servantPathEnv body
    {-
    case r of
        (view, Nothing) -> do
            liftIO $ print view
            pure $ Page.registerPage view overLimit
        (_, Just (botStatus, registration)) ->
            case botStatus of
                Form.IsBot -> redirectTo "/success"
                Form.IsHuman -> do
                    let to = (M.fromJust $ Form.participantEmail registration >>= Domain.mkMailAddress, Form.participantName registration)
                    let email = Mailer.Mail "hallo! :)" "some subject" to
                    liftIO $ Mailer.sendMail mailerHandle email
                    liftIO $ putStrLn $ show email
                    liftIO $ Db.saveRegistration conn registration
                    redirectTo "/success"
        -}

deleteRegistrationsHandler :: Db.Handle -> () -> ParticipantId -> Handler Page.Html
deleteRegistrationsHandler conn _ (ParticipantId participantId) = do
    liftIO $ Db.deleteRegistration conn (Db.DbId participantId)
    redirectTo "/admin"

printRegistrationsHandler :: Db.Handle -> () -> Handler Page.Html
printRegistrationsHandler conn _ = do
    regs <- liftIO $ Db.allRegistrationsOrderedByName conn
    pure $ Page.registrationPrintPage regs


successHandler :: Handler Page.Html
successHandler = do
    pure Page.successPage

redirectTo :: BS.ByteString -> Handler a
redirectTo url =
    throwError $ err303 { errHeaders = [("Location", url)] }

servantPathEnv :: (Monad m) => [(T.Text, T.Text)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv body _ = pure env
  where
    lookupParam p = lookup (DF.fromPath p) body
    env path = return (DF.TextInput <$> (M.maybeToList (lookupParam path)))


mailForRegistration :: D.ExistingRegistration -> Mailer.Mail
mailForRegistration registration = Mailer.Mail mailBody subject (mailAddress, firstParticipantName)
  where
    (DT.Name firstParticipantName) = P.participantName $ NE.head $ D.participants registration
    mailAddress = DT.MailAddress $ D.email registration
    subject = "Bestellbestätigung Freiburger Jonglierfestival"
    newLine = "\n\n"
    totalPrice = T.pack $ show $ D.priceToPay registration
    (DT.PaymentCode paymentReason) = D.paymentCode registration
    nameAndTicketLine p =
        let
            (P.Ticket _ age stay price) = P.participantTicket p
            (DT.Name name) = P.participantName p
        in
            "* " <> name <> " " <> P.ageLabel age <> " " <> P.stayLabel stay <> " " <> T.pack (show price)
    mailBody = T.intercalate newLine
        [ "Liebe/r " <> firstParticipantName
        , "du hast für das 21. Freiburger Jonglierfestival folgende Tickets bestellt:"
        , ""
        , T.intercalate newLine $ NE.toList $ fmap nameAndTicketLine (D.participants registration)
        , ""
        , ""
        , "bitte überweise das Geld dafür bis zum 05.05.2019 auf unser Konto:"
        , "Empfänger: Jonglieren in Freiburg e.V."
        , "Bank: Sparkasse Freiburg Nördlicher Breisgau"
        , "IBAN: DE26 6805 0101 0012 0917 91"
        , "BIC: FRSPDE66XXX"
        , "Betrag: " <> totalPrice
        , "Verwendungszweck: " <> paymentReason
        , ""
        , "Wir freuen uns Dich auf dem Festival zu sehen."
        , "Viele Grüße Dein"
        , "Orgateam"
        ]
