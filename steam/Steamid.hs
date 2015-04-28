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
import Data.Char (ord)
import Data.Text (pack)
import GHC.Int

--build the id database
--sqlite3 ids.db "CREATE TABLE ids (id INTEGER PRIMARY KEY, appid TEXT, name TEXT);"

--curl this to take a json list of all steam app ids
--http://api.steampowered.com/ISteamApps/GetAppList/v0001/



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





populateDb = do
	response <- snd <$> curlGetString ("http://api.steampowered.com/ISteamApps/GetAppList/v0001/")[]
	let response' = filter (\x -> (ord x) <= 127) $ map (\x -> if x=='·' then '-'; else x)  response
	--print response'
	let apps = decode (fromString response')
        -- let listGames = app $ applist $ app $ fromJust $ (apps :: Maybe AppList)
        let listGames = extractSteamApp . fromJust $ (apps :: Maybe AppList)
     	
	conn <- open "steam/ids.db"
	execute_ conn "BEGIN;"
	mapM_ (addApp conn) listGames
	execute_ conn "COMMIT;"
	close conn
	

--using steam/ids.db, returns the game's steam appid 
getSteamId name = do
	conn <- open "steam/ids.db"
	result <- query_ conn ("SELECT id, name FROM ids WHERE name='" <> name <>"';") :: IO [SteamApp]
	close conn
	return (result)


getAppByName::String -> IO ()
getAppByName name = do
	game <- getSteamId (fromString name::Query)
	print $ appid (head game)
	







