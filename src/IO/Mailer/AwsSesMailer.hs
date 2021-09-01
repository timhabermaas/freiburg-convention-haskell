{-# LANGUAGE OverloadedStrings #-}

module IO.Mailer.AwsSesMailer
    ( withConfig
    ) where

import Network.AWS.SES.SendEmail (sendEmail)
import IO.Mailer.Internal (Handle(..), Mail(Mail))
import Network.AWS.SES.Types (message, content, body, bText, destination, dToAddresses, dCCAddresses)
import Network.AWS.Lens
import Network.AWS (newEnv, Credentials(..), within, Region (Frankfurt), send, runResourceT, runAWS)
import Data.String (IsString(fromString))
import Domain.SharedTypes (fromMailAddress)

withConfig :: (String, String) -> (Handle -> IO a) -> IO a
withConfig keys f = do
    handle <- new keys
    f handle

new :: (String, String) -> IO Handle
new awsKeys = pure $ Handle { sendMail = sendMail' awsKeys }

sendMail' :: (String, String) -> Mail -> IO ()
sendMail' (accessKey, secretAccessKey) (Mail mailBody mailSubject (mailToAddress, _mailToName))  = do
    env <- newEnv $ FromKeys (fromString accessKey) (fromString secretAccessKey)
    let request = sendEmail "Jonglieren in Freiburg e.V. <orga@jonglieren-in-freiburg.de>" destination' message'
    response <- runResourceT $ runAWS env $ within Frankfurt $ send request
    print response
    pure ()
  where
    message' = message content' body'
    content' = content mailSubject
    body' = bText %~ const (Just bodyContent) $ body
    bodyContent = content mailBody
    destination' = dCCAddresses %~ const ["orga@jonglieren-in-freiburg.de"] $
                   dToAddresses %~ const [fromMailAddress mailToAddress] $
                   destination
