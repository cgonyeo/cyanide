{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cyanide.Data.Recipes where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Control.Monad
import Data.Time.Clock

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Cyanide.Data.Ingredients
import Cyanide.Data.IngredientClasses

getRecipes :: DBConn -> IO [Recipe]
getRecipes conn = do
    results <- P.query_ conn 
        "SELECT recipes.id                       \
       \      , recipes.name                     \
       \      , recipes.instructions             \
       \      , recipes.for_ingredient_id        \
       \ FROM recipes                            \
       \ WHERE recipes.for_ingredient_id IS NULL \
       \ ORDER BY recipes.id ASC"
    forM results $ fillInIngredient conn

fillInIngredient :: DBConn -> (Int,Maybe T.Text,T.Text,Maybe Int) -> IO Recipe
fillInIngredient conn (id,_,instr,Just ingredientId) = do
    ingredient <- getIngredient conn ingredientId
    return $ Recipe id (Right ingredient) instr
fillInIngredient _ (id,Just name,instr,_) =
    return $ Recipe id (Left name) instr
fillInIngredient _ (id,Nothing,instr,Nothing) =
    return $ Recipe id (Left "") instr

getRecipesUsingIngredientClass :: DBConn -> T.Text -> IO [Recipe]
getRecipesUsingIngredientClass conn c = do
    results <- P.query conn 
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ INNER JOIN ingredients_to_recipes      \
       \ ON recipes.id = ingredients_to_recipes.recipe_id \
       \ INNER JOIN ingredient_classes          \
       \ ON ingredients_to_recipes.ingredient_class_id = ingredient_classes.id \
       \ WHERE ingredient_classes.name = ? \
       \ ORDER BY recipes.id ASC" (P.Only c)
    forM results $ fillInIngredient conn

getRecipesUsingIngredient :: DBConn -> Ingredient -> IO [Recipe]
getRecipesUsingIngredient conn (Ingredient i _ _ _ _ _) = do
    results <- P.query conn 
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ INNER JOIN ingredients_to_recipes      \
       \ ON recipes.id = ingredients_to_recipes.recipe_id \
       \ WHERE ingredients_to_recipes.ingredient_id = ? \
       \ ORDER BY recipes.id ASC" (P.Only i)
    forM results $ fillInIngredient conn

getRecipeForIngredient :: DBConn -> Ingredient -> IO (Maybe Recipe)
getRecipeForIngredient conn (Ingredient i _ _ _ _ _) = do
    rs <- P.query conn 
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ WHERE recipes.for_ingredient_id = ?" (P.Only i)
    case rs of
        [r] -> do
            recipe <- fillInIngredient conn r
            return $ Just recipe
        _ -> return Nothing

getGlassForRecipe :: DBConn -> Recipe -> IO (Maybe Glass)
getGlassForRecipe conn (Recipe i _ _) = do
    gs <- P.query conn
        "SELECT glasses.id                            \
       \      , glasses.name                          \
       \ FROM recipes                                 \
       \ INNER JOIN recipes_to_glasses                \
       \ ON recipes_to_glasses.recipe_id = recipes.id \
       \ INNER JOIN glasses                           \
       \ ON recipes_to_glasses.glass_id = glasses.id  \
       \ WHERE recipes.id = ?"
       (P.Only i)
    case gs of
        [g] -> return $ Just g
        _   -> return Nothing

getIngredientsForRecipe :: DBConn -> Recipe -> IO [IngredientListItem]
getIngredientsForRecipe conn (Recipe i _ _) = do
    ingList :: [(Maybe Int,Maybe Int,Int,Int,T.Text)] <- P.query conn
        "SELECT ingredient_id        \
       \      , ingredient_class_id  \
       \      , amount_numer         \
       \      , amount_denom         \
       \      , unit                 \
       \ FROM ingredients_to_recipes \
       \ WHERE recipe_id = ?" (P.Only i)
    forM ingList $ \(mIngId,mClassId,num,den,u) -> 
        case (mIngId,mClassId) of
            (Just ingId,Nothing) -> do
                ing <- getIngredient conn ingId
                return $ IngredientListItem num den u (Left ing)
            (Nothing,Just classId) -> do
                cla <- getIngredientClass conn classId
                return $ IngredientListItem num den u (Right cla)

-- Create a new recipe with the given name, instructions, if it's for an
-- ingredient and not a cocktail, and either an ingredient ID on the left or an
-- ingredient class ID on the right
newRecipe :: DBConn -> (Maybe T.Text,T.Text,Maybe Ingredient,[IngredientListItem]) -> IO Recipe
newRecipe conn (mName,instr,mIngr,ingredients) = do
    [P.Only (recId :: Int)] <- P.query conn "INSERT INTO recipes (name,instructions,for_ingredient_id) VALUES (?,?,?) RETURNING id" (mName,instr,mIngr >>= Just . ingredientId)
    forM_ ingredients $ \(IngredientListItem num den unit ingOrCla) ->
        case ingOrCla of
            Left ing -> do
                P.execute conn "INSERT INTO ingredients_to_recipes (recipe_id,ingredient_id,amount_numer,amount_denom,unit) VALUES (?,?,?,?,?)" (recId,ingredientId ing,num,den,unit)
                return ()
            Right ingCla -> do
                P.execute conn "INSERT INTO ingredients_to_recipes (recipe_id,ingredient_class_id,amount_numer,amount_denom,unit) VALUES (?,?,?,?,?)" (recId,ingredientClassId ingCla,num,den,unit)
                return ()
    let recName = case (mIngr,mName) of
                (Just i,_) -> Right i
                (_,Just n) -> Left n
                (Nothing,Nothing) -> Left ""
    return $ Recipe recId recName instr

deleteRecipe :: DBConn -> Recipe -> IO ()
deleteRecipe conn (Recipe i _ _) = do
    P.execute conn "DELETE FROM recipes WHERE id = ?" (P.Only i)
    return ()
