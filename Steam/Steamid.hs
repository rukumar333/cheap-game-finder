{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleInstances #-}
module Steam.Steamid where

import Database.SQLite.Simple
import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import Network.Curl
import Control.Monad
import Data.Aeson
import Data.Char (ord)
import Data.Text (pack)
import GHC.Int
import Text.Regex

--build the id database
--sqlite3 ids.db "CREATE TABLE ids (id INTEGER PRIMARY KEY, appid INTEGER, name TEXT);"

--curl this to take a json list of all steam app ids
--http://api.steampowered.com/ISteamApps/GetAppList/v0001/

-- curl this too
--"http://store.steampowered.com/api/appdetails?appids="



-- data SteamApp = SteamApp { appid :: Int, name :: String} deriving (Show)


data SteamApp = SteamApp { appid :: Int, name :: String} deriving (Show)

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
instance FromRow SteamApp where
	fromRow = SteamApp <$> field <*> field

instance ToRow SteamApp where
	toRow steamapp = [SQLInteger (fromIntegral $ appid steamapp), SQLText (pack $ name steamapp)]


extractSteamApp :: AppList -> [SteamApp]
extractSteamApp  	=  idlist . app . applist

addApp connection  = do
   	execute connection ("INSERT INTO ids (appid, name) VALUES (?,?)")

--this function filters out illegal characters from a JSON String, and formats some other stuff
formatJSON :: String -> String
formatJSON	= removeTM .filter (\x -> (ord x) <= 127) . map (\x -> if x=='·' then '-' else x)
	where removeTM json = subRegex (mkRegex "[(](TM)[)]") json ""




populateDb = do
	response <- snd <$> curlGetString ("http://api.steampowered.com/ISteamApps/GetAppList/v0001/")[]
	let response' = formatJSON response
	--print response'
	let apps = decode (fromString response')
        -- let listGames = app $ applist $ app $ fromJust $ (apps :: Maybe AppList)
        let listGames = extractSteamApp . fromJust $ (apps :: Maybe AppList)
     	
	conn <- open "Steam/ids.db"
	execute_ conn "BEGIN;"
	mapM_ (addApp conn) listGames
	execute_ conn "COMMIT;"
	close conn
	




--using steam/ids.db, returns the games json 
--there is currently no error handling!
getAppByName::String -> IO [SteamApp]
getAppByName name = do
	games <- getSteamId (fromString name::Query)
        return games
	
--queries the database and returns an appid
getSteamId name = do
	conn <- open "Steam/ids.db"
	result <- query_ conn ("SELECT appid, name FROM ids WHERE name LIKE '%" <> name <>"%';") :: IO [SteamApp]
	close conn
	return (result)





