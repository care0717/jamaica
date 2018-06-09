import Data.List
import Data.Char 
import Text.Regex
import Text.Printf
import System.IO 

makeExpression ::[Int] -> [String] -> [String]-> [String]
makeExpression  _ [] dp = dp
makeExpression cp op dp = (take (head cp) op) ++ head dp : makeExpression (tail cp) (drop (head cp) op) (tail dp)


stringOperator f x y = show (f (read x :: Double) (read y :: Double))

calcExpression' xs = scanr calc' [[]] xs
  where calc' :: String -> [String] -> [String]
        calc' "+" (x : y : xs) = stringOperator (+) x y : xs
        calc' "-" (x : y : xs) = stringOperator (-) x y : xs
        calc' "*" (x : y : xs) = stringOperator (*) x y : xs
        calc' "/" (x : y : xs) = stringOperator (/) x y : xs
        calc' a xs = a:xs

afterDecimalPoint :: Double -> Double
afterDecimalPoint a = a - fromIntegral(floor a)       


aboutIntToInt :: Double -> Integer
aboutIntToInt a 
  | afterDecimalPoint a <= error && afterDecimalPoint a >= -error = round a
  | a < 1 = 0
  | otherwise = 0
  where error = 0.000000000001


calcExpression :: Foldable t => t String -> Integer
calcExpression xs = aboutIntToInt . read . head $ foldr calc' [[]] xs
  where calc' :: String -> [String] -> [String]
        calc' "+" (x : y : xs) = stringOperator (+) x y :xs
        calc' "-" (x : y : xs) = stringOperator (-) x y:xs
        calc' "*" (x : y : xs) = stringOperator (*) x y:xs
        calc' "/" (x : y : xs) = stringOperator (/) x y:xs
        calc' a xs = a:xs

jadgeJamaica ans a = a == ans


print_jamaica input = printf "%d %d %d %d %d %d %d %s\n" ans (int_dice !! 0) (int_dice !! 1) (int_dice !! 2) (int_dice !! 3) (int_dice !! 4) (sorted_res_count) (show $ last reas)
  where input_list = (words input)
        dice = tail input_list 
        ans = read $ head input_list :: Integer
        dice_pattern = nub $ permutations dice
        calc = [0..4]
        calc_pattern = [[d,c,b,a] | a<-calc, b<-calc, c<-calc, d<-calc, a+b+c+d==4, a<=1 && a+b<=2 && a+b+c<=3 && d>=1]
        operator = ["+","-","*","/"] :: [String]
        operator_pattern = [ [a,b,c,d] | a <- operator , b <- operator, c <- operator, d <- operator]
        reas = [ makeExpression cp op dp | cp <- calc_pattern, op <-operator_pattern, dp <- dice_pattern, jadgeJamaica ans (calcExpression $ makeExpression cp op dp) ]
        sorted_res_count = length reas 
        int_dice = map (read::String->Int) dice

main :: IO ()
main = do
  filename <- getLine
  handle <- openFile ("./text/"++filename) ReadMode
  input_string <- hGetContents handle
  let inputs = lines input_string
  mapM print_jamaica inputs
  hClose handle

