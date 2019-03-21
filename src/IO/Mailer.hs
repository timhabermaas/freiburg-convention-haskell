module IO.Mailer
    ( withConfig
    , Config(..)
    , Handle(..)
    , Mail(..)
    ) where

import qualified IO.Mailer.SendGridMailer as SendGridMailer
import qualified IO.Mailer.InMemoryMailer as InMemoryMailer
import qualified IO.Mailer.PrinterMailer as PrinterMailer
import IO.Mailer.Internal

data Config
    = InMemoryConfig
    | SendGridConfig String
    | PrinterConfig

withConfig :: Config -> (Handle -> IO a) -> IO a
withConfig (InMemoryConfig) = \callback -> InMemoryMailer.withConfig $ \(h, _) -> callback h
withConfig (SendGridConfig key) = SendGridMailer.withConfig key
withConfig (PrinterConfig) = PrinterMailer.withConfig
