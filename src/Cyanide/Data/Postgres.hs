{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cyanide.Data.Postgres where

import Data.Word
import qualified Data.Text as T
import Database.PostgreSQL.Simple as P

type DBConn = P.Connection

newDBConn :: T.Text -> Word16 -> T.Text -> T.Text -> T.Text -> IO DBConn
newDBConn h po u pa d = P.connect (P.ConnectInfo (t h) po (t u) (t pa) (t d))
    where t = T.unpack
