#!runghc
import Data.List
main = interact (unlines.map unwords.groupBy f.words.filter (/='\0')) where
	f "0xff" _ = False
	f _ _ = True
