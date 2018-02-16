module Main where

import Cyanide.Data.Postgres
import Cyanide.Data.Types
import Cyanide.Data.Glasses
import Cyanide.Data.Purchases
import Cyanide.Data.Recipes

import qualified Cyanide.UI.App as App

main :: IO ()
main = do
  conn <- newDBConn
  --recipes <- getRecipes conn
  --print recipes
  --glass <- getGlassForRecipe conn (recipes !! 0)
  --print glass
  --ings <- getIngredientsForRecipe conn (recipes !! 0)
  --print ings
  App.run conn
