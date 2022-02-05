module IO.Mailer.PrinterMailer
  ( withConfig,
  )
where

import qualified Data.Text.IO as TIO
import IO.Mailer.Internal (Handle (..), Mail (..))

new :: IO Handle
new = pure $ Handle $ sendMail'

sendMail' :: Mail -> IO ()
sendMail' mail = do
  putStrLn "==Sending mail=="
  TIO.putStrLn $ mailBody mail
  putStrLn "==End Sending mail=="

withConfig :: (Handle -> IO a) -> IO a
withConfig f = do
  h <- new
  f h
