{-# LANGUAGE DeriveGeneric #-}

module Cyanide.Data.Types where

import GHC.Generics

import Database.PostgreSQL.Simple as P
import qualified Data.Text as T
import Data.Time.Calendar

newtype CyanideErr = CyanideErr String
    deriving(Show)

data IngredientUnit = Zero
                    | Ml750
    deriving(Show,Read)

data RecipeUnit = Undefined
                | Ounce
                | Millileter
                | Dash
                | Teaspoon
    deriving(Show,Read)

data IngredientClass = IngredientClass
    { ingredientClassId   :: Int
    , ingredientClassName :: T.Text
    } deriving(Generic,Show)
instance FromRow IngredientClass

data Ingredient = Ingredient
    { ingredientId    :: Int
    , ingredientName  :: T.Text
    , ingredientClass :: T.Text
    , amount          :: Int
    , unit            :: T.Text
    , notForRecipes   :: Bool
    }
    deriving(Generic,Show)
instance FromRow Ingredient

data Glass = Glass
    { glassId   :: Int
    , glassName :: T.Text
    }
    deriving(Generic,Show)
instance FromRow Glass

data Purchase = Purchase
    { date       :: Day
    , location   :: T.Text
    , price      :: Int
    }
    deriving(Generic,Show)
instance FromRow Purchase

data Recipe = Recipe
    { recipeId        :: Int
    , recipeName      :: Either T.Text Ingredient
    -- Human readable instructions
    , instructions    :: T.Text
    }
    deriving(Generic,Show)

data IngredientListItem = IngredientListItem
    { amountNumer     :: Int
    , amountDenom     :: Int
    , ingListItemUnit :: T.Text
    , ingListItemIng  :: Either Ingredient IngredientClass
    }
    deriving(Show)
