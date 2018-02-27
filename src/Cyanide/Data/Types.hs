{-# LANGUAGE DeriveGeneric #-}

module Cyanide.Data.Types where

import GHC.Generics

import Database.PostgreSQL.Simple as P
import qualified Data.Text as T
import Data.Time.Calendar

newtype CyanideErr = CyanideErr String
    deriving(Show,Eq)

data IngredientUnit = Zero
                    | Ml750
    deriving(Show,Read,Eq)

data RecipeUnit = Undefined
                | Ounce
                | Millileter
                | Dash
                | Teaspoon
    deriving(Show,Read,Eq)

data IngredientClass = IngredientClass
    { ingredientClassId   :: Int
    , ingredientClassName :: T.Text
    } deriving(Generic,Show,Eq)
instance FromRow IngredientClass

data Ingredient = Ingredient
    { ingredientId    :: Int
    , ingredientName  :: T.Text
    , ingredientClass :: Maybe IngredientClass
    , available       :: Bool
    , notForRecipes   :: Bool
    }
    deriving(Generic,Show,Eq)

data Glass = Glass
    { glassId   :: Int
    , glassName :: T.Text
    }
    deriving(Generic,Show,Eq)
instance FromRow Glass

data Purchase = Purchase
    { date       :: Day
    , location   :: T.Text
    , price      :: Int
    , volume     :: Int
    , unit       :: T.Text
    }
    deriving(Generic,Show,Eq)
instance FromRow Purchase

data Recipe = Recipe
    { recipeId        :: Int
    , recipeName      :: Either T.Text Ingredient
    , recipeGarnish   :: T.Text
    -- Human readable instructions
    , instructions    :: T.Text
    }
    deriving(Generic,Show,Eq)

data IngredientListItem = IngredientListItem
    { amountNumer     :: Int
    , amountDenom     :: Int
    , ingListItemUnit :: T.Text
    , ingListItemIng  :: Either Ingredient IngredientClass
    }
    deriving(Show,Eq)
