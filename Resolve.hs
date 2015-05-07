
import Data.Metric.String
import Data.Metric.Class as M

--you need this package
--sudo apt-get install liblapack-dev

main = do
	word1 <- getLine
	word2 <- getLine
	let a = Levenshtein word1
	let b = Levenshtein word2
	print $ M.distance a b



--computes how many words the two strings have in common
compareWords::String -> String -> Int
compareWords a b = let list1 = words a
                       list2 = words b
                   in compareWords' 0 list1 list2
  where compareWords' :: Int -> [String] -> [String] -> Int
        compareWords' count [] _ = count
        compareWords' count (x:xs) s | x `elem` s = compareWords' (count+1) xs s
                                     | otherwise = compareWords' count xs s

