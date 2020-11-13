{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}


module Main where

import Control.Monad.Reader (runReaderT, liftIO)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import Data.Proxy
import Database.Esqueleto (select, from, insert, fromSqlKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, Entity,
                                    entityIdToJSON, entityIdFromJSON)
import Database.Persist.TH
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Util

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note
    name String
    contents String
List
    name String
    items [String]
|]

deriveJSON defaultOptions {fieldLabelModifier = decapitalize . (drop 4)} ''Note
deriveJSON defaultOptions ''List

instance ToJSON (Entity Note) where
  toJSON = entityIdToJSON

instance FromJSON (Entity Note) where
  parseJSON = entityIdFromJSON

type Crud = "notes" :> (Get '[JSON] [Entity Note] :<|>
                        ReqBody '[JSON] Note :> Post '[JSON] Int64)

crudAPI :: Proxy Crud
crudAPI = Proxy

-- TODO configure logger with timestamps if possible
runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

getNotes :: ConnectionString -> Handler [Entity Note]
getNotes conn = liftIO $ runAction conn $ (select . from $ \notes -> return notes)

postNote :: ConnectionString -> Note -> Handler Int64
postNote conn note = do
  result <- liftIO $ runAction conn $ insert note
  return $ fromSqlKey result

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=darius password=blah dbname=note_server"

noteServer :: ConnectionString -> Server Crud
noteServer conn = getNotes conn :<|> postNote conn

main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> 
    runReaderT (runMigration migrateAll) backend
  run 8080 (serve crudAPI (noteServer connString))
