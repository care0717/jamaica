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
import JamaicaEntity as Jamaica

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

runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB info = runNoLoggingT . runResourceT . withMySQLConn info . runSqlConn

connInfo :: ConnectInfo
connInfo = defaultConnectInfo { connectHost = "127.0.0.1", connectPort = 3306, connectUser = "test", connectPassword = "secret", connectDatabase = "jamaica" }

server :: ByteString -> Server API
server indexHtml = index
  :<|> serveDirectoryFileServer "server/static"
  :<|> getAnswers 
    where
      index              = pure indexHtml
      getAnswers ans d1 d2 d3 d4 d5 = liftIO (selectAnswers ans d1 d2 d3 d4 d5)

selectAnswers :: Int -> Int -> Int -> Int -> Int -> Int -> IO [Jamaica]
selectAnswers ans d1 d2 d3 d4 d5 = do
    answerList <- runDB connInfo $ selectList [JamaicaAnswer ==. ans, JamaicaDice1 ==. d1, JamaicaDice2 ==. d2, JamaicaDice3 ==. d3, JamaicaDice4 ==. d4, JamaicaDice5 ==. d5] []
    return $ map (\(Entity _ u) -> u) answerList


main :: IO ()
main = do
  indexHtml <- B.readFile "server/templates/index.html"
  putStrLn "Listening on port 8080"
  run 8080 $ serve api (server indexHtml)
