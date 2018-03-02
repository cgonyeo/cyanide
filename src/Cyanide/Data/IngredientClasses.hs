{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cyanide.Data.IngredientClasses where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T
import Control.Monad

import Cyanide.Data.Types
import Cyanide.Data.Postgres

getIngredientClasses :: DBConn -> IO [IngredientClass]
getIngredientClasses conn = P.query_ conn
            "SELECT id, name FROM ingredient_classes ORDER BY name ASC"

getIngredientClass :: DBConn -> Int -> IO IngredientClass
getIngredientClass conn classId = do
    [c] <- P.query conn
            "SELECT id, name FROM ingredient_classes WHERE id=? ORDER BY id ASC" (P.Only classId)
    return c

newIngredientClass :: DBConn -> T.Text -> IO IngredientClass
newIngredientClass conn className = do
    [P.Only i] <- P.query conn "INSERT INTO ingredient_classes (name) VALUES (?) RETURNING (id)" (P.Only className)
    return $ IngredientClass i className

isIngredientClassInUse :: DBConn -> IngredientClass -> IO Bool
isIngredientClassInUse conn ic = do
    is :: [(Int,Int)] <- P.query conn
                " SELECT ingredients.id \
                \      , ingredients.class \
                \ FROM ingredients \
                \ WHERE ingredients.class = ?" (P.Only $ ingredientClassId ic)
    rs :: [(Int,Int)] <- P.query conn
                " SELECT recipe_id \
                \      , ingredient_class_id \
                \ FROM ingredients_to_recipes \
                \ WHERE ingredient_class_id = ?" (P.Only $ ingredientClassId ic)
    return $ length is > 0 || length rs > 0

updateIngredientClass :: DBConn -> Int -> T.Text -> IO IngredientClass
updateIngredientClass conn icId className = do
    void $ P.execute conn "UPDATE ingredient_classes SET name = ? WHERE id = ?" (className,icId)
    return $ IngredientClass icId className

deleteIngredientClass :: DBConn -> IngredientClass -> IO ()
deleteIngredientClass conn (IngredientClass i _) =
    void $ P.execute conn "DELETE FROM ingredient_classes WHERE id = ?" (P.Only i)
