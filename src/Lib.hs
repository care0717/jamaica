{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Lib where

import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Database.Persist
import           Database.Persist.MySQL       (ConnectInfo (..),
                                               SqlBackend (..),
                                               defaultConnectInfo, runMigration,
                                               runSqlPool, withMySQLConn)
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           GHC.Word
import           JamaicaEntity as Jamaica
import System.Random
data Env = Env { port :: GHC.Word.Word16, user :: String, pass :: String, database :: String }

runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB info = runNoLoggingT . runResourceT . withMySQLConn info . runSqlConn

connInfo :: Env -> ConnectInfo
connInfo env = defaultConnectInfo { connectHost     = "127.0.0.1"
                                  , connectPort     = (port env)
                                  , connectUser     = (user env)
                                  , connectPassword = (pass env)
                                  , connectDatabase = (database env)
                                  }

selectAnswers :: Env -> Int -> Int -> Int -> Int -> Int -> Int -> IO [Jamaica]
selectAnswers env ans d1 d2 d3 d4 d5 = do
  answerList <- runDB (connInfo env) $ selectList
    [ JamaicaAnswer ==. ans
    , JamaicaDice1 ==. d1
    , JamaicaDice2 ==. d2
    , JamaicaDice3 ==. d3
    , JamaicaDice4 ==. d4
    , JamaicaDice5 ==. d5
    ]
    []
  return $ map (\(Entity _ u) -> u) answerList

boundaryList =
  [[1, 10, 379], [10, 100, 2390], [100, 1000, 3747], [1000, 1000000, 869]]

selectRandomProblem :: Env -> Int -> IO Jamaica
selectRandomProblem env level = do
  let (min, max, size) = case level of
        3 -> (1, 10, 379)
        2 -> (10, 100, 2390)
        1 -> (100, 1000, 3747)
        _ -> (1000, 1000000, 869)
  answerList <- runDB (connInfo env) $ selectList
    [JamaicaSolution_number >=. min, JamaicaSolution_number <. max]
    []
  gen0 <- newStdGen
  let (r1, _) = randomR (0, size - 1) gen0
  return $ (map (\(Entity _ u) -> u) answerList) !! r1



insertAnswer :: Env -> Jamaica -> IO ()
insertAnswer env = runDB (connInfo env) . insert_



















