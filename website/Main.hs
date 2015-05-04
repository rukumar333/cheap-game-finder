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
import Data.Monoid
import Control.Applicative
import Web.Scotty
import Control.Monad.IO.Class
import Data.String
import qualified Prices.Walmart as W 
import qualified Prices.BestBuy as B

checkWalmartBestBuy :: W.Game -> B.Game -> Bool
checkWalmartBestBuy wm bb = ((D.isInfixOf (W.name' wm) (B.name bb)) || (D.isInfixOf (B.name bb) (W.name' wm))) && ((W.platform' wm) == (B.platform bb))
-- main :: IO ()
main = scotty 3000 $ do
         get "/css/index.css" $ file "website/css/index.css"
         get "/bootstrap/css/bootstrap.min.css" $ file "website/bootstrap/css/bootstrap.min.css"
         get "/bootstrap/js/bootstrap.min.js" $ file "website/bootstrap/js/bootstrap.min.js"
         get "/res/background.jpg" $ file "website/res/background.jpg"
         get "/" $ file "website/index.html"

         post "/postname" $ do
                       fn <- param "full-name"
                       ea <- param "email-address"
                       pn <- param "phone-number"  
                       -- conn <- liftIO $ open "test.db"
                       -- liftIO $ execute conn "INSERT INTO users (name, email, phone) VALUES (?,?,?)" (fn,ea,pn)
                       -- html $ "Submitted"
                       html . renderText $ 
                            html_ $ 
                                  body_ $ do
                                    h1_ $ fromString fn
                                    h1_ $ fromString ea
                                    h1_ $ fromString pn

         post "/search" $ do
                       gn <- param "game"
                       gp <- param "platform"
                       bestBuyList <- liftIO $ B.getBestBuy gn gp                     
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
                                                        a_ [class_ "navbar-brand", href_ "#"] "Cheap Game Finder"
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
                                                                   option_ "PS3"
                                                                   option_ "PS4"
                                                                   option_ "XBox 360"
                                                                   option_ "XBox One"
                                                                   option_ "Wii"
                                                                   option_ "Wii U"
                                                             input_ [type_ "text", class_ "form-control input-lg", placeholder_ "Ex: Skyrim", name_ "game"]
                                                             button_ [class_ "btn btn-primary btn-lg", type_ "submit"] "Search"
                                        div_ [class_ "row"] $ do
                                             div_ [class_ "col-xs-1 col-md-3"] $ do
                                                  div_ [class_ "ui-game thumbnail"] $ do
                                                       img_ [class_ "ui-game-cover img-responsive", src_ "http://upload.wikimedia.org/wikipedia/en/8/89/Dragon_Age_Origins_cover.png"]
                                                       div_ [class_ "caption"] $ do
                                                            h3_ (fromString gn)
                                                            div_ [class_ "website-price"] $ do
                                                                 a_ [href_ "#"] "Best Buy: "
                                                                 p_ "$60.00"
                                                            div_ [class_ "website-price"] $ do
                                                                 a_ [href_ "#"] "Walmart: "
                                                                 p_ "$60.00"
                                        toHtmlRaw ("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js\"></script>" :: Text)
                                        toHtmlRaw ("<script src=\"bootstrap/js/bootstrap.min.js\"></script>" :: Text)

    -- <div class="row">
    --   <div id="wide-search-bar" class="text-center">
    --     <div id="form-header">
    --       <h1 id="form-header-text">Search for another game!</h1>
    --     </div>
    --     <form class="navbar-form" role="search" method="post" action="/search">
    --       <div class="form-group">
    --         <select class="form-control input-lg" name="platform">
    --           <option>Platform</option>
    --           <option>PS3</option>
    --           <option>PS4</option>
    --           <option>XBox 360</option>
    --           <option>XBox One</option>
    --           <option>Wii</option>
    --           <option>Wii U</option>
    --         </select>
    --         <input type="text" class="form-control input-lg" placeholder="Ex: Skyrim" name="game">
    --         <button type="button" class="btn btn-primary btn-lg" type="submit">Search</button>
    --       </div>     
    --     </form>
    --   </div>
    -- </div>
    -- <div class="row">
    --   <div class="col-xs-1 col-md-3">
    --     <div class="ui-game thumbnail">
    --       <img class="ui-game-cover img-responsive" src="http://upload.wikimedia.org/wikipedia/en/8/89/Dragon_Age_Origins_cover.png">
    --       <div class="caption">
    --         <h3>Dragon Age Origins</h3>
    --         <div class="website-price">
    --           <a href="#">Best Buy:</a>
    --           <p>$60.00</p>
    --         </div>
    --         <div class="website-price">
    --           <a href="#">Wal Mart:</a>
    --           <p>$50.00</p>
    --         </div>
    --       </div>
    --     </div>
    --   </div>
    -- </div>

         
                  
