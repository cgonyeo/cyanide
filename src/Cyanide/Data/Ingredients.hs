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
       \      , ingredients.notForRecipes             \
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
       \      , ingredients.notForRecipes             \
       \ FROM ingredients                             \
       \ INNER JOIN ingredient_classes                \
       \ ON ingredients.class = ingredient_classes.id \
       \ WHERE ingredients.id = ?                     \
       \ ORDER BY ingredients.id ASC" (P.Only ingredientId)
    return i

newIngredient :: DBConn -> (T.Text,IngredientClass,T.Text,Bool) -> IO Ingredient
newIngredient conn (n,(IngredientClass ci c),u,s) = do
    [P.Only i] <- P.query conn "INSERT INTO ingredients (name,class,amount,unit,notForRecipes) VALUES (?,?,?,?,?) RETURNING (id)" (n,ci,0 :: Int,u,s)
    return $ Ingredient i n c 0 u s

updateIngredient :: DBConn -> Int -> (T.Text,IngredientClass,T.Text,Bool) -> IO Ingredient
updateIngredient conn i (n,(IngredientClass ci c),u,s) = do
    P.execute conn "UPDATE ingredients SET name = ?, class = ?, unit = ?, notForRecipes = ? WHERE id = ?" (n,ci,u,s,i)
    getIngredient conn i

updateIngredientAmount :: DBConn -> (Ingredient,Int) -> IO ()
updateIngredientAmount conn ((Ingredient i _ _ _ _ _),a) = do
    P.execute conn "UPDATE ingredients SET amount = ? WHERE id = ?" (a,i)
    return ()

deleteIngredient :: DBConn -> Ingredient -> IO ()
deleteIngredient conn (Ingredient i _ _ _ _ _) = do
    P.execute conn "DELETE FROM ingredients WHERE id = ?" (P.Only i)
    return ()
