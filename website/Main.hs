{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lucid
import Lucid.Base
import Data.Text.Internal
import qualified Data.Text as D
import Database.SQLite.Simple
import Data.Functor
import Data.Functor.Identity
import Data.List
import qualified Data.Char as Ch
import Data.Monoid
import Control.Applicative
import Web.Scotty
import Control.Monad.IO.Class
import Data.String
import qualified Prices.Walmart as W 
import qualified Prices.BestBuy as B
import qualified Steam.Steam as S
import qualified Gamesdata.GamesDB as G

data UniversalGame = UniversalGame
    {
      name :: D.Text,
      price :: Double,
      platform :: D.Text,
      productUrl :: D.Text,
      largeImage :: D.Text,
      store :: D.Text
    }
                     deriving (Show)

instance Eq UniversalGame where
    (==) game game' = (price game) == (price game')

instance Ord UniversalGame where
    -- (<) game game' = (price game) < (price game')    
    -- (>) game game' = (price game) > (price game')    
    compare game game' = compare (price game) (price game')
    

walmartToUniversal :: W.Game -> IO UniversalGame
walmartToUniversal game = return $ UniversalGame (W.name' game) (W.price' game) (W.platform' game) (W.productUrl' game) (W.largeImage' game) ("Walmart: $")

bestBuyToUniversal :: B.Game -> IO UniversalGame
bestBuyToUniversal game = return $ UniversalGame (B.name game) (B.price game) (B.platform game) (B.productUrl game) (B.frontImage game) ("Best Buy: $")

steamToUniversal :: S.Game -> IO UniversalGame
steamToUniversal game = return $ UniversalGame (D.pack $ S.name' game) (S.price' game) "PC" "" (D.pack $ S.image game) ("Steam: $")

checkWalmartBestBuy :: W.Game -> B.Game -> Bool
checkWalmartBestBuy wm bb = ((D.isInfixOf (W.name' wm) (B.name bb)) || (D.isInfixOf (B.name bb) (W.name' wm))) && ((W.platform' wm) == (B.platform bb))

createObjectUniversal :: UniversalGame -> Html ()
createObjectUniversal game = div_ [class_ "col-xs-1 col-md-3"] $ do
                               div_ [class_ "ui-game thumbnail"] $ do
                                   img_ [class_ "ui-game-cover img-responsive", src_ $ largeImage game]
                                   div_ [class_ "caption"] $ do
                                       htmlName game
                                       div_ [class_ "website-price"] $ do
                                           p_ (fromString $ ((D.unpack $ store game) ++ (show $ price game)))

                                                             where htmlName :: UniversalGame -> Html ()   
                                                                   htmlName gm   | (store gm) == "Steam: $" = h3_ (fromString $ D.unpack $ name gm)
                                                                   htmlName gm   | otherwise                = a_ [href_ (fromString $ D.unpack $ productUrl gm)] $ h3_ (fromString $ D.unpack $ name gm) 

universalListToHtml :: [UniversalGame] -> Html ()
universalListToHtml []     = do
                              div_ [] ""
universalListToHtml (x:xs) = do
                              createObjectUniversal x
                              universalListToHtml xs


createObjectWalmart :: W.Game -> Html ()
createObjectWalmart game = div_ [class_ "col-xs-1 col-md-3"] $ do
                               div_ [class_ "ui-game thumbnail"] $ do
                                   img_ [class_ "ui-game-cover img-responsive", src_ $ W.largeImage' game]
                                   div_ [class_ "caption"] $ do
                                       a_ [href_ (fromString $ D.unpack $ W.productUrl' game)] $ h3_ (fromString $ D.unpack $ W.name' game)
                                       div_ [class_ "website-price"] $ do
                                           p_ (fromString $ ("Walmart: $" ++ (show $ W.price' game)))

walmartListToHtml :: [W.Game] -> Html ()
walmartListToHtml []     = do
                              div_ [] ""
walmartListToHtml (x:xs) = do
                              createObjectWalmart x
                              walmartListToHtml xs

createObjectBestBuy :: B.Game -> Html ()
createObjectBestBuy game = div_ [class_ "col-xs-1 col-md-3"] $ do
                               div_ [class_ "ui-game thumbnail"] $ do
                                   img_ [class_ "ui-game-cover img-responsive", src_ $ B.frontImage game]
                                   div_ [class_ "caption"] $ do
                                       a_ [href_ (fromString $ D.unpack $ B.productUrl game)] $ h3_ (fromString $ D.unpack $ B.name game)
                                       div_ [class_ "website-price"] $ do
                                           p_ (fromString $ ("Best Buy: $" ++ (show $ B.price game)))

bestBuyListToHtml :: [B.Game] -> Html ()
bestBuyListToHtml []     = do
                              div_ [] ""
bestBuyListToHtml (x:xs) = do
                              createObjectBestBuy x
                              bestBuyListToHtml xs

createObjectSteam :: S.Game -> Html ()
createObjectSteam game = div_ [class_ "col-xs-1 col-md-3"] $ do
                               div_ [class_ "ui-game thumbnail"] $ do
                                   -- imageURL <- liftIO $ G.getUrl (S.name game) "PC"                            
                                   img_ [class_ "ui-game-cover img-responsive", src_ (fromString (S.image game))]
                                   div_ [class_ "caption"] $ do
                                       h3_ (fromString $ S.name' game)
                                       div_ [class_ "website-price"] $ do
                                           p_ (fromString $ ("Steam: $" ++ (show $ S.price' game)))
                                           -- liftIO $ print game

steamListToHtml :: [S.Game] -> Html ()
steamListToHtml []     = do
                              div_ [] ""
steamListToHtml (x:xs) = do
                              createObjectSteam x
                              -- (liftIO $ print x) :: Html ()
                              steamListToHtml xs

upperCaseWord :: String -> String
upperCaseWord [] = ""
upperCaseWord (x:xs) = (Ch.toUpper x) : (map Ch.toLower xs)

upperCaseWords :: String -> String
upperCaseWords listWords = unwords (map upperCaseWord (words listWords))

-- main :: IO ()
main = scotty 3000 $ do
         get "/css/index.css" $ file "website/css/index.css"
         get "/bootstrap/css/bootstrap.min.css" $ file "website/bootstrap/css/bootstrap.min.css"
         get "/bootstrap/js/bootstrap.min.js" $ file "website/bootstrap/js/bootstrap.min.js"
         get "/res/background.jpg" $ file "website/res/background.jpg"
         get "/" $ file "website/index.html"

         post "/search" $ do
                       gn <- param "game"
                       gp <- param "platform"
                       bestBuyList <- liftIO $ B.getBestBuy gn gp                     
                       walmartList' <- liftIO $ W.getWalmart gn gp                     
                       steamList' <- liftIO $ S.getGameBySubName gn gp
                       steamList <- liftIO $ mapM S.fromSteamGame steamList'
                       walmartList <- liftIO $ mapM W.fixImages walmartList'
                       universalList <- ((<$>) sort) $ liftIO $ liftA3 (\x y z -> x ++ y ++ z) (mapM steamToUniversal steamList) (mapM bestBuyToUniversal bestBuyList) (mapM walmartToUniversal walmartList)
                       html . renderText $
                            html_ [lang_ "en"] $ do
                                  head_ $ do
                                        meta_ [charset_ "utf-8"]
                                        meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE-edge"]
                                        title_ "Game Finder"
                                        link_ [href_ "bootstrap/css/bootstrap.min.css", rel_ "stylesheet", type_ "text/css"]
                                        link_ [href_ "css/index.css", rel_ "stylesheet", type_ "text/css"]

                                  body_ $ do
                                        nav_ [class_ "navbar navbar-inverse"] $ do
                                             div_ [class_ "container-fluid"] $ do
                                                   div_ [class_ "navbar-header"] $ do
                                                        a_ [class_ "navbar-brand", href_ "http://localhost:3000/"] "Cheap Game Finder"
                                        div_ [class_ "background"] ""
                                        div_ [class_ "row"] $ do 
                                              div_ [id_ "wide-search-bar", class_ "text-center"] $ do
                                                   div_ [id_ "form-header"] $ do
                                                        h1_ [id_ "form-header-text"] "Search for another game!"
                                                   form_ [class_ "navbar-form", method_ "post", action_ "/search"] $ do
                                                        div_ [class_ "form-group"] $ do
                                                             select_ [class_ "form-control input-lg", name_ "platform"] $ do
                                                                   option_ "Platform"
                                                                   option_ "PC"
                                                                   option_ "PlayStation 3"
                                                                   option_ "PlayStation 4"
                                                                   option_ "Xbox 360"
                                                                   option_ "Xbox One"
                                                                   option_ "Wii"
                                                                   option_ "Wii U"
                                                             input_ [type_ "text", class_ "form-control input-lg", placeholder_ "Ex: Skyrim", name_ "game"]
                                                             button_ [class_ "btn btn-primary btn-lg", type_ "submit"] "Search"
                                        div_ [class_ "row"] $ do 
                                              showPlatform gn gp
                                        div_ [class_ "row"] $ do
                                              -- universalListToHtml universalList
                                              populateResults gn gp universalList
                                        toHtmlRaw ("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js\"></script>" :: Text)
                                        toHtmlRaw ("<script src=\"bootstrap/js/bootstrap.min.js\"></script>" :: Text)

                           where populateResults :: String -> String -> [UniversalGame] -> Html ()
                                 populateResults game platform results | platform == "Platform" = div_ [id_ "wide-search-bar", class_ "text-center"] $ do
                                                                                                      div_ [id_ "form-header"] $ do
                                                                                                          h1_ [id_ "form-header-text"] "Please select a platform." 
                                                                       | null results           = div_ [id_ "wide-search-bar", class_ "text-center"] $ do
                                                                                                      div_ [id_ "form-header"] $ do
                                                                                                          h1_ [id_ "form-header-text"] "No games found. Please search for another game."
                                                                       | otherwise              = universalListToHtml results
                                 showPlatform :: String -> String -> Html ()
                                 showPlatform game platform | platform == "Platform" = div_ ""
                                                            | otherwise              = div_ [id_ "wide-search-bar", class_ "text-center"] $ do
                                                                                           div_ [id_ "form-header"] $ do
                                                                                               h2_ [id_ "form-header-text"] $ fromString $ ("Showing results for " ++ (upperCaseWords game) ++ " (" ++ platform ++ ").")
