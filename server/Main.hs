{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//), (/:))
import qualified Data.ByteString.Lazy as B
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
import JamaicaEntity as Jamaica
import Lib (Env(..), runDB, connInfo, selectAnswers)
import System.Environment (getEnv, getArgs)


type API = Get '[HTML] ByteString
         :<|> "static" :> Raw
         :<|> JamaicaAPI

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ bs = bs

api :: Proxy API
api = Proxy

doMigration :: Env -> IO ()
doMigration env = runNoLoggingT $ runResourceT $ withMySQLConn (connInfo env) $ runReaderT $ runMigration migrateAll

server :: Env -> ByteString -> Server API
server env indexHtml = index
  :<|> serveDirectoryFileServer "server/static"
  :<|> getAnswers 
    where
      index              = pure indexHtml
      getAnswers ans d1 d2 d3 d4 d5 = liftIO (selectAnswers env ans d1 d2 d3 d4 d5)

main :: IO ()
main = do
  port <- getEnv "JAMAICA_PORT"
  user <- getEnv "JAMAICA_USER"
  pass <- getEnv "JAMAICA_PASS"
  database <- getEnv "JAMAICA_DATABASE"
  listenPort <- getEnv "JAMAICA_LISTENPORT"
  let env = Env {port = (read port :: GHC.Word.Word16), user = user, pass = pass, database = database}
  args <- getArgs
  let arg1 = if (length args > 0) then Just (args !! 0) else Nothing
  indexHtml <- B.readFile "server/templates/index.html"
  
  putStrLn $ concat ["Listening on port ", listenPort]
  case arg1 of
      Just "migrate" -> doMigration env
      _ -> run (read listenPort) $ serve api (server env indexHtml)
  
