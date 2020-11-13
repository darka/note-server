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

module Main where

import Control.Monad.Reader (runReaderT, liftIO)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Data.Aeson.TH
import Data.Proxy
import Database.Esqueleto (entityVal, select, from, insert)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
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

type Crud = "notes" :> (Get '[JSON] [Note] :<|>
                        ReqBody '[JSON] Note :> Post '[JSON] Note)

crudAPI :: Proxy Crud
crudAPI = Proxy

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

getNotes :: ConnectionString -> Handler [Note]
getNotes conn = do
  notes <- liftIO $ runAction conn $ (select . from $ \notes -> return notes)
  return $ map entityVal notes

postNote :: ConnectionString -> Note -> Handler Note
postNote conn note = do
  liftIO $ runAction conn $ insert note
  return note

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=darius password=blah dbname=note_server"

noteServer :: ConnectionString -> Server Crud
noteServer conn = getNotes conn :<|> postNote conn

main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> 
    runReaderT (runMigration migrateAll) backend
  run 8080 (serve crudAPI (noteServer connString))
