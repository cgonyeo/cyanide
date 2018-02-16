{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Ingredients where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Data.Time.Clock

getIngredients :: DBConn -> IO [Ingredient]
getIngredients conn = P.query_ conn 
        "SELECT ingredients.id                        \
       \      , ingredients.name                      \
       \      , ingredient_classes.name               \
       \      , ingredients.amount                    \
       \      , ingredients.unit                      \
       \ FROM ingredients                             \
       \ INNER JOIN ingredient_classes                \
       \ ON ingredients.class = ingredient_classes.id \
       \ ORDER BY ingredients.id ASC"

getIngredient :: DBConn -> Int -> IO Ingredient
getIngredient conn ingredientId = do
    [i] <- P.query conn 
        "SELECT ingredients.id                        \
       \      , ingredients.name                      \
       \      , ingredient_classes.name               \
       \      , ingredients.amount                    \
       \      , ingredients.unit                      \
       \ FROM ingredients                             \
       \ INNER JOIN ingredient_classes                \
       \ ON ingredients.class = ingredient_classes.id \
       \ WHERE ingredients.id = ?                     \
       \ ORDER BY ingredients.id ASC" (P.Only ingredientId)
    return i

newIngredient :: DBConn -> (T.Text,IngredientClass) -> IO ()
newIngredient conn (n,(IngredientClass i _)) = do
    P.execute conn "INSERT INTO ingredients (class,name,amount,unit) VALUES (?,?,?,?)" (i,n,0 :: Int,show Zero)
    return ()

updateIngredientAmount :: DBConn -> (Ingredient,Int) -> IO ()
updateIngredientAmount conn ((Ingredient i _ _ _ _),a) = do
    P.execute conn "UPDATE ingredients SET amount = ? WHERE id = ?" (a,i)
    return ()

deleteIngredient :: DBConn -> Ingredient -> IO ()
deleteIngredient conn (Ingredient i _ _ _ _) = do
    P.execute conn "DELETE FROM ingredients WHERE id = ?" (P.Only i)
    return ()
