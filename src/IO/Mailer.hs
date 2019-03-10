module IO.Mailer
    ( withConfig
    , Config(..)
    , Handle(..)
    , Mail(..)
    ) where

import qualified IO.Mailer.SendGridMailer as SendGridMailer
import qualified IO.Mailer.InMemoryMailer as InMemoryMailer
import IO.Mailer.Internal

data Config
    = InMemoryConfig
    | SendGridConfig String

withConfig :: Config -> (Handle -> IO a) -> IO a
withConfig (InMemoryConfig) = \callback -> InMemoryMailer.withConfig $ \(h, _) -> callback h
withConfig (SendGridConfig key) = SendGridMailer.withConfig key
