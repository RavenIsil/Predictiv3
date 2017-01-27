{-# LANGUAGE OverloadedStrings,DeriveGeneric, UnicodeSyntax #-}

module TwitterQuery (
      twitterSearch
    , User(..)
    , Status(..)
    , Search(..)
)where

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


myoauth :: OAuth
myoauth =
     newOAuth { oauthServerName     = "api.twitter.com"
              , oauthConsumerKey    = "jrN6WRCDpNL0Wc3JQVZxPpj2m"
              , oauthConsumerSecret = "DsdWVoFpvvE6R4bInZUWULEltwoZlMVAjvcj7zSIkOr9A69PH4"
              }

mycred :: Credential
mycred = newCredential "2483402131-VrHqBvZZkjAhYQMVoNJPs1cc7BjWq3AOiixM3qT"
                          "mlyJx7LTThsV5HgLoxKbowVgN5B2l8uue6PJo6Si6fNsD"

data User = User { screen_name :: !String } deriving (Show,Generic)

data Status = Status { text :: !String, lang :: !String, user :: !User } deriving (Show,Generic)

data Search = Search { statuses :: ![Status] } deriving (Show,Generic)

instance FromJSON User
instance ToJSON User
instance FromJSON Status
instance ToJSON Status
instance FromJSON Search
instance ToJSON Search

twitterSearch :: String -> IO (Either String Search)
twitterSearch term = do
     req <- parseRequest $ "https://api.twitter.com/1.1/search/tweets.json?count=100&q=a%20lang%3A" ++ term
     res <- withManager $ \m -> do
             signedreq <- signOAuth myoauth mycred req
             httpLbs signedreq m
     return $ eitherDecode $ responseBody res
