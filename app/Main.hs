module Main where

import Lib
import System.Environment (getEnv, lookupEnv)
import Data.Text (pack)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    port <- read <$> getEnv "PORT"
    sleepingLimit <- read <$> getEnv "SLEEPING_LIMIT"
    campingLimit <- read <$> getEnv "CAMPING_LIMIT"
    pw <- AdminPassword . pack <$> getEnv "ADMIN_PASSWORD"
    sendGridApiKey <- lookupEnv "SENDGRID_API_KEY"
    awsAccessKey <- lookupEnv "AWS_ACCESS_KEY_ID"
    awsSecretKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
    startApp dbUrl port sleepingLimit campingLimit pw ((,) <$> awsAccessKey <*> awsSecretKey)
