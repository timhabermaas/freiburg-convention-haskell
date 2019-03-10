{-# LANGUAGE OverloadedStrings #-}

module IO.Mailer.SendGridMailer
    ( withConfig
    ) where

import Control.Monad (void)
import qualified Network.SendGridV3.Api as Client
import qualified Data.Text as T
import Domain.SharedTypes (MailAddress(MailAddress))
import Data.List.NonEmpty (fromList)
import IO.Mailer.Internal (Handle(..), Mail(..))
import Control.Exception (bracket)

new :: String -> IO Handle
new key =
    pure $ Handle { sendMail = sendMail' key }

sendMail' :: String -> Mail -> IO ()
sendMail' key mail = void $ Client.sendMail (Client.ApiKey $ T.pack key) $ getSendgridMail mail

close :: Handle -> IO ()
close _ = pure ()

withConfig :: String -> (Handle -> IO a) -> IO a
withConfig key f = bracket (new key) close f

from :: Client.MailAddress
from = Client.MailAddress "info@jonglieren-in-freiburg.de" "Jonglieren in Freiburg e.V."

domainToClient :: MailAddress -> T.Text -> Client.MailAddress
domainToClient (MailAddress text) name = Client.MailAddress text name
domainToClient _ _ = undefined -- TODO: Why does COMPLETE not work when defining patterns?

getSendgridMail :: Mail -> Client.Mail () ()
getSendgridMail (Mail body subject (address, name)) =
    Client.mail [Client.personalization $ fromList [domainToClient address name]] from subject (fromList [Client.mailContentText body])
