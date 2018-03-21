import Data.List
import Data.Char 

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


aboutIntToInt :: Double -> Double
aboutIntToInt a 
  | afterDecimalPoint a <= error && afterDecimalPoint a >= -error = fromIntegral(round a)
  | a < 1 = 0.0
  | otherwise = a
  where error = 0.000000000001


calcExpression :: Foldable t => t String -> Double
calcExpression xs = aboutIntToInt . read . head $ foldr calc' [[]] xs
  where calc' :: String -> [String] -> [String]
        calc' "+" (x : y : xs) = stringOperator (+) x y :xs
        calc' "-" (x : y : xs) = stringOperator (-) x y:xs
        calc' "*" (x : y : xs) = stringOperator (*) x y:xs
        calc' "/" (x : y : xs) = stringOperator (/) x y:xs
        calc' a xs = a:xs

jadgeJamaica a = a `elem` list
  where list = [fromIntegral(a+b) | a<-[10,20..60], b<-[1..6]]

main :: IO ()
main = do
  let dice = map show [1..6] :: [String]
  let dice_pattern = [[a, b, c, d, e]| a <- dice , b <-dice, c<-dice, d<-dice, e<-dice] 
  let calc = [0..4]
  let calc_pattern = [[d,c,b,a] | a<-calc, b<-calc, c<-calc, d<-calc, a+b+c+d==4, a<=1 && a+b<=2 && a+b+c<=3 && d>=1]
  let operator = ["+","-","*","/"] :: [String]
  let operator_pattern = [ [a,b,c,d] | a <- operator , b <- operator, c <- operator, d <- operator]
  let res = filter jadgeJamaica [calcExpression(makeExpression cp op dp) | cp <- calc_pattern, op <-operator_pattern, dp <- dice_pattern]
  let reas = nub [round(calcExpression(makeExpression cp op dp)):(sort(map read dp)) | cp <- calc_pattern, op <-operator_pattern, dp <- dice_pattern, jadgeJamaica . calcExpression $ makeExpression cp op dp ]

  writeFile "out.txt" (show reas)

  print "finish"
