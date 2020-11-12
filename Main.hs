{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
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

module Main where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Note
    name String
    contents String
List
    name String
    items [String]
|]

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=darius password=blah dbname=note_server"


main :: IO ()
main = do
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> 
    runReaderT (runMigration migrateAll) backend
