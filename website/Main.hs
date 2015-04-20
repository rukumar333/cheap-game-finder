{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lucid
import Lucid.Base
import Data.Monoid
import Control.Applicative
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
         
         get "/" $ file "./index.html"
