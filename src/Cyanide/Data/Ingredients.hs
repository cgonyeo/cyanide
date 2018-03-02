{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Ingredients where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Cyanide.Data.IngredientClasses
import Control.Monad

loadClass :: DBConn -> (Int,T.Text,Maybe Int,Bool,Bool) -> IO Ingredient
loadClass _ (ingId,name,Nothing,avail,notForRec) = return $ Ingredient ingId name Nothing avail notForRec
loadClass conn (ingId,name,Just classId,avail,notForRec) = do
    ic <- getIngredientClass conn classId
    return $ Ingredient ingId name (Just ic) avail notForRec

getIngredients :: DBConn -> IO [Ingredient]
getIngredients conn = do
    ingData <- P.query_ conn
        "SELECT ingredients.id                        \
       \      , ingredients.name                      \
       \      , ingredients.class                     \
       \      , ingredients.available                 \
       \      , ingredients.notForRecipes             \
       \ FROM ingredients                             \
       \ ORDER BY ingredients.name ASC"
    forM ingData (loadClass conn)

getIngredient :: DBConn -> Int -> IO Ingredient
getIngredient conn ingId = do
    [i] <- P.query conn
        "SELECT ingredients.id                        \
       \      , ingredients.name                      \
       \      , ingredients.class                     \
       \      , ingredients.available                 \
       \      , ingredients.notForRecipes             \
       \ FROM ingredients                             \
       \ WHERE ingredients.id = ?                     \
       \ ORDER BY ingredients.id ASC" (P.Only ingId)
    loadClass conn i

newIngredient :: DBConn -> (T.Text,Maybe IngredientClass,Bool) -> IO Ingredient
newIngredient conn (n,mic,s) = do
    let micId = mic >>= (Just . ingredientClassId)
    [P.Only i] <- P.query conn "INSERT INTO ingredients (name,class,available,notForRecipes) VALUES (?,?,?,?) RETURNING (id)" (n,micId,False,s)
    loadClass conn (i,n,micId,False,s)

updateIngredient :: DBConn -> Int -> (T.Text,Maybe IngredientClass,Bool) -> IO Ingredient
updateIngredient conn i (n,mic,s) = do
    let micId = mic >>= (Just . ingredientClassId)
    void $ P.execute conn "UPDATE ingredients SET name = ?, class = ?, notForRecipes = ? WHERE id = ?" (n,micId,s,i)
    getIngredient conn i

updateIngredientAvailability :: DBConn -> (Ingredient,Bool) -> IO ()
updateIngredientAvailability conn (i,a) =
    void $ P.execute conn "UPDATE ingredients SET available = ? WHERE id = ?" (a,ingredientId i)

deleteIngredient :: DBConn -> Ingredient -> IO ()
deleteIngredient conn i =
    void $ P.execute conn "DELETE FROM ingredients WHERE id = ?" (P.Only (ingredientId i))
