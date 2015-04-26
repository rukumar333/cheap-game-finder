{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleInstances #-}
import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import Network.Curl
import Control.Monad
import Data.Aeson

--build the id database
--sqlite3 ids.db "CREATE TABLE app_ids (id INTEGER PRIMARY KEY, appid TEXT);"

--curl this to take a json list of all steam app ids
--http://api.steampowered.com/ISteamApps/GetAppList/v0001/



data SteamApp = SteamApp { appid :: Double, name :: String} deriving (Show)

data AppList = AppList { applist :: Apps} deriving (Show)

data Apps = Apps {app :: App} deriving (Show)

data App = App {idlist :: [SteamApp]} deriving (Show)

--end of data

instance FromJSON AppList where
    parseJSON (Object v) = AppList
                           <$> v .: "applist"
instance FromJSON Apps where
    parseJSON (Object v) = Apps
                           <$> v .: "apps"
instance FromJSON App where
    parseJSON (Object v) = App
                           <$> v .: "app"
              
         

instance FromJSON SteamApp where
    parseJSON (Object v) = SteamApp
                           <$> v .: "appid"
                           <*> v .: "name"


run = do
	response <- snd <$> curlGetString "http://api.steampowered.com/ISteamApps/GetAppList/v0001/" []
	let apps = decode (fromString response)
        -- let listGames = app $ applist $ app $ fromJust $ (apps :: Maybe AppList)
        let listGames = fromJust $ (apps :: Maybe AppList)
        print listGames
	-- print "nothing"





