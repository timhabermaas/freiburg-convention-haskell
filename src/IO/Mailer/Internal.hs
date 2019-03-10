module IO.Mailer.Internal
    ( Handle(..)
    , Mail(..)
    ) where

import qualified Data.Text as T
import Domain.SharedTypes

data Mail
    = Mail
    { mailBody :: T.Text
    , mailSubject :: T.Text
    , mailTo :: (MailAddress, T.Text)
    } deriving Show

data Handle
    = Handle
    { sendMail :: Mail -> IO ()
    }

