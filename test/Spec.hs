{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app, AdminPassword(..), Config(..))
import Types
import Test.Hspec
import Test.Hspec.Wai
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy as T
import Data.Semigroup ((<>))

import qualified IO.Db as Db
import qualified IO.Mailer.InMemoryMailer as InMemoryMailer
import qualified IO.Mailer.Internal as Internal
import System.Environment (getEnv)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    Db.withConfig dbUrl $ \dbHandle -> do
        InMemoryMailer.withConfig $ \(mailHandle, readMailList) -> do
            Db.migrate dbHandle -- TODO: Reset database
            hspec $ spec dbHandle mailHandle readMailList (AdminPassword "admin")

spec :: Db.Handle -> Internal.Handle -> IO [Internal.Mail] -> AdminPassword -> Spec
spec dbHandle mailHandle readMailList pw = do
    let limit = (GymSleepingLimit 2, CampingSleepingLimit 0)
    with (return $ app (Config mailHandle dbHandle pw limit)) $ do
        describe "GET /" $ do
            it "responds with 200" $ do
                get "/" `shouldRespondWith` 200
            it "has some FAQ entries" $ do
                get "/" `shouldRespondWith` (successAndContains "Warum muss ich mich dieses Jahr überhaupt anmelden?")

successAndContains :: T.Text -> ResponseMatcher
successAndContains text =
    200 { matchBody = MatchBody matcher }
  where
    matcher _header body =
        if T.isInfixOf text (TE.decodeUtf8 body)
            then Nothing
            else Just $ show text <> " not found"
