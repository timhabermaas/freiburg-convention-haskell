module IO.Mailer
    ( withConfig
    , Config(..)
    , Handle(..)
    , Mail(..)
    ) where

import qualified IO.Mailer.AwsSesMailer as AwsSesMailer
import qualified IO.Mailer.InMemoryMailer as InMemoryMailer
import qualified IO.Mailer.PrinterMailer as PrinterMailer
import IO.Mailer.Internal

data Config
    = InMemoryConfig
    | AwsSesConfig (String, String)
    | PrinterConfig

withConfig :: Config -> (Handle -> IO a) -> IO a
withConfig (InMemoryConfig) = \callback -> InMemoryMailer.withConfig $ \(h, _) -> callback h
withConfig (AwsSesConfig keys) = AwsSesMailer.withConfig keys
withConfig (PrinterConfig) = PrinterMailer.withConfig
withConfig (PrinterConfig) = PrinterMailer.withConfig
