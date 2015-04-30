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
import Text.Regex
import Steam.Steamid as S

--trim regex strings
--(\")([0-9]+[0-9]+)(\":{\"success\":true,)
--(}})


data SteamGame = SteamGame { name :: String, price::Double} deriving (Show)

fromSteamApp :: S.SteamApp -> Double -> SteamGame
fromSteamApp app price = SteamGame (S.name app) price

-- use this function from the cmd line
getGames :: String -> IO ()
getGames name = do
	game <- S.getAppByName name
	games <- mapM getGameAtId game
	let games' = (map (fromJust) . filter (isJust)) games
	print games'
	



getGameAtId :: S.SteamApp -> IO (Maybe SteamGame)
getGameAtId app = do
	response <- snd <$> curlGetString ("http://store.steampowered.com/api/appdetails?appids="++ (show $ S.appid app))[]
	let r = matchRegex (mkRegex "(\"final\":)([0-9]+[0-9]+)") response
	if r == Nothing
	then return Nothing
	else do
		let r' = fromJust r
		let price = (read (r' !! 1)::Double) / 100.0
		return $ Just (SteamGame (S.name app) price)
	
	
	
	
