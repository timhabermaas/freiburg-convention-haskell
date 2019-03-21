module IO.Mailer.PrinterMailer
    ( withConfig
    ) where

import IO.Mailer.Internal (Handle(..), Mail)

new :: IO Handle
new = pure $ Handle $ sendMail'

sendMail' :: Mail -> IO ()
sendMail' mail = do
    putStrLn "==Sending mail=="
    putStrLn $ show mail
    putStrLn "==End Sending mail=="

withConfig :: (Handle -> IO a) -> IO a
withConfig f = do
    h <- new
    f h
