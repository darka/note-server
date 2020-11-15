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
{-# LANGUAGE FlexibleContexts           #-}


module Main where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT, liftIO, ask)
import Control.Monad.Except (ExceptT(..))
import Data.Int (Int64)
import Data.Proxy
import Database.Esqueleto (select, from, insert, toSqlKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, Entity(..), selectFirst, (==.), createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
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
                        ReqBody '[JSON] Note :> Post '[JSON] (Entity Note)) :<|>
            "lists" :> (Get '[JSON] [Entity List] :<|>
                        Capture "listid" Int64 :> Get '[JSON] (Maybe (Entity List)) :<|>
                        ReqBody '[JSON] List :> Post '[JSON] (Entity List))

crudAPI :: Proxy Crud
crudAPI = Proxy

data Env = Env { pool :: ConnectionPool }

newtype AppMonad a = AppMonad { unAppMonad :: ReaderT Env IO a } deriving
  (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runAction :: (MonadReader Env m, MonadIO m) => SqlPersistT IO a -> m a
runAction action = do
  env <- ask
  liftIO . runSqlPool action $ pool env

getNotes :: AppMonad [Entity Note]
getNotes = runAction $ (select . from $ \notes -> return notes)

getNote :: Int64 -> AppMonad (Maybe (Entity Note))
getNote noteId = runAction $ selectFirst [NoteId ==. toSqlKey noteId] []

postNote :: Note -> AppMonad (Entity Note)
postNote note = do
  key <- runAction $ insert note
  return $ Entity key note

getLists :: AppMonad [Entity List]
getLists = runAction $ (select . from $ \lists -> return lists)

getList :: Int64 -> AppMonad (Maybe (Entity List))
getList listId = runAction $ selectFirst [ListId ==. toSqlKey listId] []

postList :: List -> AppMonad (Entity List)
postList list = do
  key <- runAction $ insert list
  return $ Entity key list

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=darius password=blah dbname=note_server"

noteServer :: ServerT Crud AppMonad
noteServer = (getNotes :<|> getNote :<|> postNote) :<|>
             (getLists :<|> getList :<|> postList)

hoistAppServer :: Env -> Server Crud
hoistAppServer config = hoistServer crudAPI (nt config) noteServer where
  nt :: Env -> AppMonad a -> Handler a
  nt env m = Handler $ ExceptT $ try $ runReaderT (unAppMonad m) env

main :: IO ()
main = do
  -- TODO configure logger with timestamps if possible
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> 
    runReaderT (runMigration migrateAll) backend
  dbPool <- runStdoutLoggingT $ createPostgresqlPool connString 2
  run 8080 (serve crudAPI (hoistAppServer $ Env { pool = dbPool }))
