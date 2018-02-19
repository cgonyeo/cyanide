{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Glasses where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres

getGlasses :: DBConn -> IO [Glass]
getGlasses conn = P.query_ conn "SELECT id, name FROM glasses ORDER BY id ASC"

newGlass :: DBConn -> T.Text -> IO Glass
newGlass conn glassName = do
    [P.Only i] <- P.query conn "INSERT INTO glasses (name) VALUES (?) RETURNING (id)" (P.Only glassName)
    return $ Glass i glassName

updateGlass :: DBConn -> Int -> T.Text -> IO Glass
updateGlass conn id glassName = do
    P.execute conn "UPDATE glasses SET name = ? WHERE id = ?" (glassName, id)
    return $ Glass id glassName

deleteGlass :: DBConn -> Glass -> IO ()
deleteGlass conn (Glass i _) = do
    P.execute conn "DELETE FROM glasses WHERE id = ?" (P.Only i)
    return ()
