{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleInstances #-}
module Resolve where
import Data.Metric.String
import Data.Metric.Class as M
import Data.Monoid
import qualified Data.Text as D

--you need this package
--sudo apt-get install liblapack-dev





--computes how many words the two strings have in common
compareWords::String -> String -> Int
compareWords a b = let list1 = words a
                       list2 = words b
                   in compareWords' 0 list1 list2
  where compareWords' :: Int -> [String] -> [String] -> Int
        compareWords' count [] _ = count
        compareWords' count (x:xs) s | x `elem` s = compareWords' (count+1) xs s
                                     | otherwise = compareWords' count xs s




--modified version of default words function		
isolate :: String -> [String]
isolate s = isolate' (words s)
	where   isolate' :: [String] -> [String]
		isolate' [] = []		
		isolate' (x:xs) | length x == 1	= (" "++ x):isolate' xs
			        | otherwise		= x : isolate' xs
