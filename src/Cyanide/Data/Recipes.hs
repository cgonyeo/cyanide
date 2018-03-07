{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cyanide.Data.Recipes where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Control.Monad
import Data.Maybe

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Cyanide.Data.Ingredients
import Cyanide.Data.IngredientClasses

getRecipes :: DBConn -> IO [Recipe]
getRecipes conn = do
    results <- P.query_ conn
        "SELECT recipes.id                       \
       \      , recipes.name                     \
       \      , recipes.garnish                  \
       \      , recipes.instructions             \
       \      , recipes.for_ingredient_id        \
       \ FROM recipes                            \
       \ WHERE recipes.for_ingredient_id IS NULL \
       \ ORDER BY recipes.name ASC"
    forM results $ fillInIngredient conn

fillInIngredient :: DBConn -> (Int,Maybe T.Text,T.Text,T.Text,Maybe Int) -> IO Recipe
fillInIngredient conn (recId,_,garnish,instr,Just ingId) = do
    ingredient <- getIngredient conn ingId
    return $ Recipe recId (Right ingredient) garnish instr
fillInIngredient _ (recId,Just name,garnish,instr,_) =
    return $ Recipe recId (Left name) garnish instr
fillInIngredient _ (recId,Nothing,garnish,instr,Nothing) =
    return $ Recipe recId (Left "") garnish instr

getRecipesUsingIngredientClass :: DBConn -> IngredientClass -> IO [Recipe]
getRecipesUsingIngredientClass conn ic = do
    results <- P.query conn
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.garnish                 \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ INNER JOIN ingredients_to_recipes      \
       \ ON recipes.id = ingredients_to_recipes.recipe_id \
       \ WHERE ingredients_to_recipes.ingredient_class_id = ? \
       \ ORDER BY recipes.name ASC" (P.Only $ ingredientClassId ic)
    forM results $ fillInIngredient conn

getRecipesUsingIngredient :: DBConn -> Ingredient -> IO [Recipe]
getRecipesUsingIngredient conn i = do
    results <- P.query conn
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.garnish                 \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ INNER JOIN ingredients_to_recipes      \
       \ ON recipes.id = ingredients_to_recipes.recipe_id \
       \ WHERE ingredients_to_recipes.ingredient_id = ? \
       \ ORDER BY recipes.name ASC" (P.Only $ ingredientId i)
    forM results $ fillInIngredient conn

getRecipeForIngredient :: DBConn -> Ingredient -> IO (Maybe Recipe)
getRecipeForIngredient conn i = do
    rs <- P.query conn
        "SELECT recipes.id                      \
       \      , recipes.name                    \
       \      , recipes.garnish                 \
       \      , recipes.instructions            \
       \      , recipes.for_ingredient_id       \
       \ FROM recipes                           \
       \ WHERE recipes.for_ingredient_id = ?" (P.Only $ ingredientId i)
    case rs of
        [r] -> do
            recipe <- fillInIngredient conn r
            return $ Just recipe
        _ -> return Nothing

getGlassForRecipe :: DBConn -> Recipe -> IO (Maybe Glass)
getGlassForRecipe conn (Recipe i _ _ _) = do
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
getIngredientsForRecipe conn (Recipe i _ _ _) = do
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
            _ -> do
                putStrLn "reached impossible state in getIngredientsForRecipe"
                undefined

getRecipesToIngredientClasses :: DBConn -> IO [(Int,Int)]
getRecipesToIngredientClasses conn = do
    res1 :: [(Int,Int)] <- P.query_ conn
        "SELECT recipes.id, ingredients_to_recipes.ingredient_class_id as icid FROM recipes INNER JOIN ingredients_to_recipes ON recipe_id = recipes.id WHERE ingredients_to_recipes.ingredient_class_id IS NOT NULL"
    res2 :: [(Int,Int)] <- P.query_ conn
        "SELECT recipes.id, ingredients.class FROM recipes INNER JOIN ingredients_to_recipes ON recipes.id = ingredients_to_recipes.recipe_id inner join ingredients on ingredients.id=ingredients_to_recipes.ingredient_id WHERE ingredients.class IS NOT NULL"
    return (res1 ++ res2)

getRecipesToGlasses :: DBConn -> IO [(Int,Int)]
getRecipesToGlasses conn = do
    P.query_ conn
        " SELECT recipes.id \
        \      , recipes_to_glasses.glass_id \
        \ FROM recipes \
        \ INNER JOIN recipes_to_glasses \
        \ ON recipes_to_glasses.recipe_id = recipes.id"

getRecipesIngredientAvailability :: DBConn -> IO [(Int,Bool)]
getRecipesIngredientAvailability conn = do
    P.query_ conn
        "SELECT query1.id, query1.avail AND query2.avail FROM (SELECT query1_1.id, coalesce(bool_and(avail),FALSE) AS avail FROM (SELECT recipes.id, ingredient_classes.id as icid, coalesce(bool_or(ingredients.available), FALSE) AS avail FROM recipes INNER JOIN ingredients_to_recipes ON recipe_id = recipes.id INNER JOIN ingredient_classes ON ingredient_class_id = ingredient_classes.id LEFT OUTER JOIN ingredients ON ingredients.class = ingredient_classes.id WHERE (ingredients.notForRecipes = FALSE OR ingredients.notForRecipes IS NULL) GROUP BY (ingredient_classes.id,recipes.id)) AS query1_1 GROUP BY query1_1.id) AS query1 INNER JOIN (SELECT recipes.id, bool_and(ingredients.available) as avail FROM recipes INNER JOIN ingredients_to_recipes ON ingredients_to_recipes.recipe_id = recipes.id INNER JOIN ingredients ON ingredient_id = ingredients.id WHERE ingredients.notForRecipes = FALSE GROUP BY recipes.id) AS query2 ON query1.id = query2.id order by query1.id"

newRecipe :: DBConn -> (Maybe T.Text,T.Text,T.Text,Maybe Glass,Maybe Ingredient,[IngredientListItem]) -> IO Recipe
newRecipe conn (mName,garnish,instr,mGlass,mIngr,ingredients) = do
    -- Create the recipe
    [P.Only (recId :: Int)] <- P.query conn "INSERT INTO recipes (name,garnish,instructions,for_ingredient_id) VALUES (?,?,?,?) RETURNING id" (mName,garnish,instr,mIngr >>= Just . ingredientId)
    createGlassAndIngredients conn recId mGlass ingredients
    let recName = case (mIngr,mName) of
                (Just i,_) -> Right i
                (_,Just n) -> Left n
                (Nothing,Nothing) -> Left ""
    return $ Recipe recId recName garnish instr

updateRecipe :: DBConn -> Int -> (Maybe T.Text,T.Text,T.Text,Maybe Glass,Maybe Ingredient,[IngredientListItem]) -> IO Recipe
updateRecipe conn recId (mName,garnish,instr,mGlass,mIngr,ingredients) = do
    -- Update the recipe
    void $ P.execute conn "UPDATE recipes SET name = ?, garnish = ?, instructions = ?, for_ingredient_id = ? WHERE id = ?" (mName,garnish,instr,mIngr >>= Just . ingredientId,recId)
    void $ P.execute conn "DELETE FROM recipes_to_glasses WHERE recipe_id = ?" (P.Only recId)
    void $ P.execute conn "DELETE FROM ingredients_to_recipes WHERE recipe_id = ?" (P.Only recId)
    createGlassAndIngredients conn recId mGlass ingredients
    let recName = case (mIngr,mName) of
                (Just i,_) -> Right i
                (_,Just n) -> Left n
                (Nothing,Nothing) -> Left ""
    return $ Recipe recId recName garnish instr

createGlassAndIngredients :: DBConn -> Int -> Maybe Glass -> [IngredientListItem] -> IO ()
createGlassAndIngredients conn recId mGlass ingredients = do
    -- If there's a glass, create it
    case mGlass of
        Nothing -> return ()
        (Just g) ->
            void $ P.execute conn "INSERT INTO recipes_to_glasses (glass_id,recipe_id) VALUES (?,?)" (glassId g,recId)

    -- Create the ingredient mappings
    forM_ ingredients $ \(IngredientListItem num den un ingOrCla) ->
        case ingOrCla of
            Left ing ->
                void $ P.execute conn "INSERT INTO ingredients_to_recipes (recipe_id,ingredient_id,amount_numer,amount_denom,unit) VALUES (?,?,?,?,?)" (recId,ingredientId ing,num,den,un)
            Right ingCla ->
                void $ P.execute conn "INSERT INTO ingredients_to_recipes (recipe_id,ingredient_class_id,amount_numer,amount_denom,unit) VALUES (?,?,?,?,?)" (recId,ingredientClassId ingCla,num,den,un)

deleteRecipe :: DBConn -> Recipe -> IO ()
deleteRecipe conn (Recipe i _ _ _) =
    void $ P.execute conn "DELETE FROM recipes WHERE id = ?" (P.Only i)
