{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cyanide.Data.Glasses where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T
import Control.Monad

import Cyanide.Data.Types
import Cyanide.Data.Postgres

getGlasses :: DBConn -> IO [Glass]
getGlasses conn = P.query_ conn "SELECT id, name FROM glasses ORDER BY id ASC"

newGlass :: DBConn -> T.Text -> IO Glass
newGlass conn gName = do
    [P.Only i] <- P.query conn "INSERT INTO glasses (name) VALUES (?) RETURNING (id)" (P.Only gName)
    return $ Glass i gName

isGlassInUse :: DBConn -> Glass -> IO Bool
isGlassInUse conn g = do
    rs :: [(Int,Int)] <- P.query conn
                " SELECT recipes_to_glasses.recipe_id \
                \      , recipes_to_glasses.glass_id \
                \ FROM glasses \
                \ INNER JOIN recipes_to_glasses \
                \ ON glasses.id = recipes_to_glasses.glass_id \
                \ WHERE glasses.id = ?" (P.Only $ glassId g)
    return $ length rs > 0

updateGlass :: DBConn -> Int -> T.Text -> IO Glass
updateGlass conn glaId gName = do
    void $ P.execute conn "UPDATE glasses SET name = ? WHERE id = ?" (gName, glaId)
    return $ Glass glaId gName

deleteGlass :: DBConn -> Glass -> IO ()
deleteGlass conn (Glass i _) =
    void $ P.execute conn "DELETE FROM glasses WHERE id = ?" (P.Only i)
