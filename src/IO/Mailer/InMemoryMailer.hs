module IO.Mailer.InMemoryMailer
  ( withConfig,
  )
where

import Data.IORef
import IO.Mailer.Internal (Handle (..), Mail)

new :: IORef [Mail] -> IO Handle
new ref = do
  pure $ Handle $ sendMail' ref

sendMail' :: IORef [Mail] -> Mail -> IO ()
sendMail' ref mail = do
  putStrLn $ "sending email" ++ show mail
  modifyIORef ref (mail :)

withConfig :: ((Handle, IO [Mail]) -> IO a) -> IO a
withConfig f = do
  ref <- newIORef []
  handle <- new ref
  f (handle, reverse <$> readIORef ref)
