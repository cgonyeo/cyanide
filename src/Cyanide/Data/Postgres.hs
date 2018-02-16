{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cyanide.Data.Postgres where

import Database.PostgreSQL.Simple as P

type DBConn = P.Connection

connectionSettings :: P.ConnectInfo
connectionSettings = P.ConnectInfo
    { connectHost     = "localhost"
    , connectPort     = 5432
    , connectUser     = "cyanide"
    , connectPassword = "up spirits"
    , connectDatabase = "cyanide"
    }

newDBConn :: IO DBConn
newDBConn = P.connect connectionSettings
