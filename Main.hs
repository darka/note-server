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
import Data.Int (Int64)
import Data.Proxy
import Database.Esqueleto (select, from, insert, toSqlKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, Entity(..), selectFirst, (==.))
import Database.Persist.TH
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note json
    name String
    contents String
List json
    name String
    items [String]
|]

type Crud = "notes" :> (Get '[JSON] [Entity Note] :<|>
                        Capture "noteid" Int64 :> Get '[JSON] (Maybe (Entity Note)) :<|>
                        ReqBody '[JSON] Note :> Post '[JSON] (Entity Note))

crudAPI :: Proxy Crud
crudAPI = Proxy

-- TODO configure logger with timestamps if possible
runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

getNotes :: ConnectionString -> Handler [Entity Note]
getNotes conn = liftIO $ runAction conn $ (select . from $ \notes -> return notes)

getNote :: ConnectionString -> Int64 -> Handler (Maybe (Entity Note))
getNote conn noteId = liftIO $ runAction conn $ selectFirst [NoteId ==. toSqlKey noteId] []

postNote :: ConnectionString -> Note -> Handler (Entity Note)
postNote conn note = do
  key <- liftIO $ runAction conn $ insert note
  return $ Entity key note

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=darius password=blah dbname=note_server"

noteServer :: ConnectionString -> Server Crud
noteServer conn = getNotes conn :<|> getNote conn :<|> postNote conn

main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> 
    runReaderT (runMigration migrateAll) backend
  run 8080 (serve crudAPI (noteServer connString))
