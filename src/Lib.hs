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

import           JamaicaEntity as Jamaica

app :: Application
app = serve jamaicaAPI server

runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB info = runNoLoggingT . runResourceT . withMySQLConn info . runSqlConn

connInfo :: ConnectInfo
connInfo = defaultConnectInfo { connectHost = "127.0.0.1", connectPort = 3306, connectUser = "test", connectPassword = "secret", connectDatabase = "jamaica" }

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runReaderT $ runMigration migrateAll

server :: Server JamaicaAPI
server = getAnswers
    where
        getAnswers ans d1 d2 d3 d4 d5 = liftIO (selectAnswers ans d1 d2 d3 d4 d5)

selectAnswers :: Int -> Int -> Int -> Int -> Int -> Int -> IO [Jamaica]
selectAnswers ans d1 d2 d3 d4 d5 = do
    answerList <- runDB connInfo $ selectList [JamaicaAnswer ==. ans, JamaicaDice1 ==. d1, JamaicaDice2 ==. d2, JamaicaDice3 ==. d3, JamaicaDice4 ==. d4, JamaicaDice5 ==. d5] []
    return $ map (\(Entity _ u) -> u) answerList

insertAnswer :: Jamaica -> IO ()
insertAnswer = runDB connInfo . insert_


