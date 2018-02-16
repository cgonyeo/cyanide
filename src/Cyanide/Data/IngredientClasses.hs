{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.IngredientClasses where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres

getIngredientClasses :: DBConn -> IO [IngredientClass]
getIngredientClasses conn = P.query_ conn
            "SELECT id, name FROM ingredient_classes ORDER BY id ASC"

getIngredientClass :: DBConn -> Int -> IO IngredientClass
getIngredientClass conn classId = do
    [c] <- P.query conn
            "SELECT id, name FROM ingredient_classes WHERE id=? ORDER BY id ASC" (P.Only classId)
    return c

newIngredientClass :: DBConn -> T.Text -> IO IngredientClass
newIngredientClass conn className = do
    [P.Only i] <- P.query conn "INSERT INTO ingredient_classes (name) VALUES (?) RETURNING (id)" (P.Only className)
    return $ IngredientClass i className

deleteIngredientClass :: DBConn -> IngredientClass -> IO ()
deleteIngredientClass conn (IngredientClass i _) = do
    P.execute conn "DELETE FROM ingredient_classes WHERE id = ?" (P.Only i)
    return ()
