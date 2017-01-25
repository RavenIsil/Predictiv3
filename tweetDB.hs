{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module TweetDB (
      createTweetsDatabase
    , insertTweetsInDatabase
    , collectTweetsIntoDatabase
) where

import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC
import Control.Concurrent
import Data.Char
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics
import TwitterQuery


queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
     conn <- connectSqlite3 databaseFile
     let result = quickQuery' conn sqlQuery []
     disconnect conn
     result

createTweetsDatabase :: IO()
createTweetsDatabase = do
       conn <- connectSqlite3 "tweets.sql"
       run conn createStatement []
       commit conn
       disconnect conn
       putStrLn "Successfully created database."
     where
       createStatement =
           "CREATE TABLE tweets (message TEXT, user TEXT, language TEXT)"


insertTweetsInDatabase :: [Status] -> IO()
insertTweetsInDatabase tweets = do
       conn <- connectSqlite3 "tweets.sql"
       stmt <- prepare conn insertStatement
       executeMany stmt sqlRecords
       commit conn
       disconnect conn
       putStrLn "Successfully inserted Tweets to database."
     where
       insertStatement = "INSERT INTO tweets VALUES (?, ?, ?)"
       sqlRecords = L.map (\(Status message language (User user)) ->
                    [toSql message, toSql user, toSql language])
                    tweets


collectTweetsIntoDatabase :: IO()
collectTweetsIntoDatabase = do
       status <- twitterSearch "en"
       either
          putStrLn
          (\(Search statuses) -> insertTweetsInDatabase statuses) status
       threadDelay 5000
