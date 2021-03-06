{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleInstances #-}
module Steam.Steam where
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
import Steam.Steamid as  S
import Data.List
import Resolve
import qualified Gamesdata.GamesDB as G

--trim regex strings
--(\")([0-9]+[0-9]+)(\":{\"success\":true,)
--(}})


data SteamGame = SteamGame { name :: String, price::Double} deriving (Show)
data Game = Game {name'::String, price'::Double, image::String} deriving (Show)



-- use this function from the cmd line
getGameByName :: String -> IO (Maybe SteamGame)
getGameByName name = do
	app <- S.getAppByName name
	game <- return $ getGameFromApp <$> app
	if isNothing game then return Nothing else fromJust game

--returns a list of steam games that contain the given substring
getGameBySubName name platform = do
	if platform == "PC" then do	
		conn <- open "games.db"
		execute_ conn "BEGIN;"	
		let myQuery = createQuery name
		apps <- query_ conn myQuery :: IO [SteamApp]
		execute_ conn "END"
		close conn
		games <- mapM getGameFromApp apps
		let results = fromJust <$> filter (isJust) games
		return results
	else return []
	
-- this is experimental: given a list of games, it attempts to return a game without the dlc by returning the game with the shortest name
ignoreDLC :: [SteamGame] -> SteamGame
ignoreDLC [x] = x
ignoreDLC games = foldr comp (head games) games
		where comp :: SteamGame -> SteamGame -> SteamGame
		      comp a b = if (length $ Steam.Steam.name a) < (length $ Steam.Steam.name b) then a else b
		       

	
	

--use to match a steam game by its exact name"
getGameFromApp :: S.SteamApp -> IO (Maybe SteamGame)
getGameFromApp app = do
	response <- snd <$> curlGetString ("http://store.steampowered.com/api/appdetails?appids="++ (show $ S.appid app))[]
	let name = S.name app
	let r = matchRegex (mkRegex "(\"final\":)([0-9]+[0-9]+)") response
	let price =  (/100) <$> (fmap read (( !! 1) <$> r):: Maybe Double)
	return $ SteamGame <$> Just name <*> price

createQuery:: String -> Query
createQuery n	= createQuery' $ (isolate . filter (\x -> not $ x `elem` ".-:–")) n
		where createQuery' tokens = 
			let  start 	= "SELECT appid, name FROM ids WHERE name LIKE '%" <> (fromString (head tokens)::Query) <> "%'"::Query
			     addtoken 	= (\x -> " AND name LIKE '%" <> (fromString x::Query) <> "%'"::Query)
			in  start <> (mconcat $ map addtoken (tail tokens)) <> ";"


--converts steamgame to game

-- fromSteamGame :: SteamGame -> String -> Game
-- fromSteamGame s url = let a = Steam.Steam.name s
--                           b = price s
--                       in Game a b url

fromSteamGame :: SteamGame ->  IO Game
fromSteamGame s = do
  imageUrl <- (G.getUrl (Steam.Steam.name s) "PC")
  return $ Game (Steam.Steam.name s) (price s) imageUrl


	
	
	
	
