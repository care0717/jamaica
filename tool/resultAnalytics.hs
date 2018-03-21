import Control.Applicative
import Data.List

grouping xs = map tail $ foldl join' [[11]] xs
  where join' (x:xs) y 
          | head x == head y = (x ++ (tail y)):xs
          | otherwise = y:x:xs

patternCount xs 
  | xs == [] = 0
  | otherwise = ordCnt (take 5 xs) + patternCount (drop 5 xs)
    where ordCnt ys = length . nub $ permutations ys

main :: IO ()
main = do
  nums <- fmap (fmap (read :: String -> Int) . words) .lines <$> readFile "result.txt"
  let groupedNum = grouping nums
  let patternCnt = reverse $ map patternCount groupedNum
  let list = [a+b | a<-[10,20..60], b<-[1..6]]

  print (zip list patternCnt)

  
