{-# LANGUAGE OverloadedStrings #-}
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



data SteamApp = SteamApp { appid::String, name::String} deriving (Show)




