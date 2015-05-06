
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
