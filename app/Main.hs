module Main where

import Lib
import Network.Wai.Handler.Warp
import System.Environment           (getArgs)

main :: IO ()
main = do
  putStrLn "start port 8080"
  args <- getArgs
  let arg1 = if (length args > 0) then Just (args !! 0) else Nothing
  case arg1 of
      Just "migrate" -> doMigration
      _ -> run 8080 app
