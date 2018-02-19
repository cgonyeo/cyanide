{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cyanide.Data.Postgres where

import Data.Word
import qualified Data.Text as T
import Database.PostgreSQL.Simple as P

initDbString =
    " BEGIN; \
    \ CREATE TABLE IF NOT EXISTS \"ingredient_classes\" ( \
    \     id   SERIAL        NOT NULL, \
    \     name VARCHAR(1024) NOT NULL, \
    \     PRIMARY KEY (id) \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"ingredients\" ( \
    \     id     SERIAL        NOT NULL, \
    \     class  INTEGER       NOT NULL, \
    \     name   VARCHAR(1024) NOT NULL, \
    \     amount INTEGER       NOT NULL, \
    \     unit   VARCHAR(1024) NOT NULL, \
    \     PRIMARY KEY (id), \
    \     FOREIGN KEY (class) REFERENCES \"ingredient_classes\" (id) ON DELETE CASCADE \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"purchases\" ( \
    \     ingredient INTEGER       NOT NULL, \
    \     date       DATE          NOT NULL, \
    \     location   VARCHAR(1024) NOT NULL, \
    \     price      INTEGER       NOT NULL, \
    \     FOREIGN KEY (ingredient) REFERENCES \"ingredients\" (id) ON DELETE CASCADE \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"recipes\" ( \
    \     id                SERIAL         NOT NULL, \
    \     name              VARCHAR(1024)  NOT NULL, \
    \     instructions      VARCHAR(10240) NOT NULL, \
    \     for_ingredient_id INTEGER UNIQUE, \
    \     PRIMARY KEY (id), \
    \     FOREIGN KEY (for_ingredient_id) REFERENCES \"ingredients\" (id) ON DELETE CASCADE \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"glasses\" ( \
    \     id   SERIAL        NOT NULL, \
    \     name VARCHAR(1024) NOT NULL, \
    \     PRIMARY KEY (id) \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"recipes_to_glasses\" ( \
    \     glass_id  INTEGER NOT NULL, \
    \     recipe_id INTEGER NOT NULL, \
    \     FOREIGN KEY (recipe_id) REFERENCES \"recipes\" (id) ON DELETE CASCADE, \
    \     FOREIGN KEY (glass_id)  REFERENCES \"glasses\" (id) ON DELETE CASCADE \
    \ ); \
    \  \
    \ CREATE TABLE IF NOT EXISTS \"ingredients_to_recipes\" ( \
    \     recipe_id          INTEGER       NOT NULL, \
    \     ingredient_id      INTEGER, \
    \     ingredient_class_id INTEGER, \
    \     amount_numer       INTEGER       NOT NULL, \
    \     amount_denom       INTEGER       NOT NULL, \
    \     unit              VARCHAR(1024) NOT NULL, \
    \     FOREIGN KEY (recipe_id)     REFERENCES \"recipes\"     (id) ON DELETE CASCADE, \
    \     FOREIGN KEY (ingredient_id) REFERENCES \"ingredients\" (id) ON DELETE CASCADE \
    \ ); \
    \ COMMIT;"

type DBConn = P.Connection

newDBConn :: T.Text -> Word16 -> T.Text -> T.Text -> T.Text -> IO DBConn
newDBConn h po u pa d = P.connect (P.ConnectInfo (t h) po (t u) (t pa) (t d))
    where t = T.unpack

initDb :: DBConn -> IO ()
initDb conn = do
    P.execute_ conn initDbString
    return ()
