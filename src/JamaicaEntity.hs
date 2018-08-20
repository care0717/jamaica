{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module JamaicaEntity where


import           Data.Aeson
import           Data.Text                    (Text)
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
import           Servant
import           Servant.API
  


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Jamaica json
    answer   Int sqltype=tinyint
    dice1      Int sqltype=tinyint
    dice2      Int sqltype=tinyint
    dice3      Int sqltype=tinyint
    dice4      Int sqltype=tinyint
    dice5      Int sqltype=tinyint
    solution_number Int
    solution_example Text
    deriving Eq Show Generic
|]

type JamaicaAPI  = "answers" :> Capture "answer" Int :> Capture "d1" Int :> Capture "d2" Int :> Capture "d3" Int :> Capture "d4" Int :> Capture "d5" Int :> Get '[JSON] [Jamaica]
            -- :<|> "users" :> Capture "name" Text :> Capture "age" Int :> Post '[JSON] ()

jamaicaAPI :: Proxy JamaicaAPI
jamaicaAPI = Proxy
