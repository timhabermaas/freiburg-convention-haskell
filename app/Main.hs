module Main where

import Lib
import System.Environment (getEnv, lookupEnv)
import Data.Text (pack)

main :: IO ()
main = do
    dbUrl <- getEnv "DATABASE_URL"
    port <- read <$> getEnv "PORT"
    overallLimit <- read <$> getEnv "OVERALL_LIMIT"
    sleepingLimit <- read <$> getEnv "SLEEPING_LIMIT"
    campingLimit <- read <$> getEnv "CAMPING_LIMIT"
    pw <- AdminPassword . pack <$> getEnv "ADMIN_PASSWORD"
    awsAccessKey <- lookupEnv "AWS_ACCESS_KEY_ID"
    awsSecretKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
    startApp dbUrl port overallLimit sleepingLimit campingLimit pw ((,) <$> awsAccessKey <*> awsSecretKey)
