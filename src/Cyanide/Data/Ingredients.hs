{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Ingredients where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Cyanide.Data.IngredientClasses
import Data.Time.Clock
import Control.Monad

loadClass :: DBConn -> (Int,T.Text,Maybe Int,Bool,Bool) -> IO Ingredient
loadClass _ (ingId,name,Nothing,avail,notForRecipes) = return $ Ingredient ingId name Nothing avail notForRecipes
loadClass conn (ingId,name,Just classId,avail,notForRecipes) = do
    ic <- getIngredientClass conn classId
    return $ Ingredient ingId name (Just ic) avail notForRecipes

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
getIngredient conn ingredientId = do
    [i] <- P.query conn
        "SELECT ingredients.id                        \
       \      , ingredients.name                      \
       \      , ingredients.class                     \
       \      , ingredients.available                 \
       \      , ingredients.notForRecipes             \
       \ FROM ingredients                             \
       \ WHERE ingredients.id = ?                     \
       \ ORDER BY ingredients.id ASC" (P.Only ingredientId)
    loadClass conn i

newIngredient :: DBConn -> (T.Text,Maybe IngredientClass,Bool) -> IO Ingredient
newIngredient conn (n,mic,s) = do
    let micId = mic >>= (Just . ingredientClassId)
    [P.Only i] <- P.query conn "INSERT INTO ingredients (name,class,available,notForRecipes) VALUES (?,?,?,?) RETURNING (id)" (n,micId,False,s)
    loadClass conn (i,n,micId,False,s)

updateIngredient :: DBConn -> Int -> (T.Text,Maybe IngredientClass,Bool) -> IO Ingredient
updateIngredient conn i (n,mic,s) = do
    let micId = mic >>= (Just . ingredientClassId)
    P.execute conn "UPDATE ingredients SET name = ?, class = ?, notForRecipes = ? WHERE id = ?" (n,micId,s,i)
    getIngredient conn i

updateIngredientAvailability :: DBConn -> (Ingredient,Bool) -> IO ()
updateIngredientAvailability conn (i,a) = do
    P.execute conn "UPDATE ingredients SET available = ? WHERE id = ?" (a,ingredientId i)
    return ()

deleteIngredient :: DBConn -> Ingredient -> IO ()
deleteIngredient conn i = do
    P.execute conn "DELETE FROM ingredients WHERE id = ?" (P.Only (ingredientId i))
    return ()
