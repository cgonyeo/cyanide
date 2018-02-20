{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.State where

import qualified Data.Text as T
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF

import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Types as Types

-- | Named resources
type Name = T.Text

data CyanideState = CyanideState Postgres.DBConn CyanideScreen

data CyanideScreen
    = MainSelectionScreen
    | GlassSelectionScreen
        { glassUIList :: BL.List Name Types.Glass
        }
    | GlassDeletionScreen
        { glassDeletionList :: BL.List Name Types.Glass
        }
    | GlassInputScreen
        { glassCreationName           :: BE.Editor T.Text Name
        , glassBeingEdited            :: Maybe Types.Glass
        , glassCreationPreviousScreen :: BL.List Name Types.Glass
        }
    | IngredientSelectionScreen
        { ingredientSelectionList :: BL.List Name Types.Ingredient
        }
    | IngredientInputScreen
        { ingredientInputName           :: BE.Editor T.Text Name
        , ingredientInputClass          :: BL.List Name Types.IngredientClass
        , ingredientInputUnit           :: BL.List Name T.Text
        , ingredientInputFocusRing      :: BF.FocusRing Name
        , ingredientInputNotForRecipes  :: Bool
        , ingredientInputBeingModified  :: Maybe Types.Ingredient
        , ingredientInputPreviousScreen :: BL.List Name Types.Ingredient
        }
    | IngredientDetailScreen
        { ingredient                     :: Types.Ingredient
        , ingredientPurchases            :: BL.List Name Types.Purchase
        , ingredientUsedIn               :: BL.List Name Types.Recipe
        , ingredientRecipe               :: Maybe Types.Recipe
        , ingredientDetailPreviousScreen :: BL.List Name Types.Ingredient
        , ingredientFocusRing            :: BF.FocusRing Name
        }
    | IngredientDeletionScreen
        { ingredientDeletionUsedIn               :: [Types.Recipe]
        , ingredientDeletionRecipe               :: Maybe Types.Recipe
        , ingredientDeletionDetailPreviousScreen :: BL.List Name Types.Ingredient
        }
    | PurchaseDeletionScreen
        { purchaseDeletionIngredient           :: Types.Ingredient
        , purchaseDeletionPurchases            :: BL.List Name Types.Purchase
        , purchaseDeletionUsedIn               :: BL.List Name Types.Recipe
        , purchaseDeletionRecipe               :: Maybe Types.Recipe
        , purchaseDeletionDetailPreviousScreen :: BL.List Name Types.Ingredient
        , purchaseDeletionFocusRing            :: BF.FocusRing Name
        }
    | PurchaseCreationScreen
        -- Fields for the IngredientDetailScreen preceeding this
        { purchaseCreationIngredient           :: Types.Ingredient
        , purchaseCreationPurchases            :: BL.List Name Types.Purchase
        , purchaseCreationUsedIn               :: BL.List Name Types.Recipe
        , purchaseCreationRecipe               :: Maybe Types.Recipe
        , purchaseCreationDetailPreviousScreen :: BL.List Name Types.Ingredient
        , purchaseCreationFocusRing            :: BF.FocusRing Name
        -- Fields for the PurchaseCreationScreen
        , purchaseCreationEditLocation         :: BE.Editor T.Text Name
        , purchaseCreationEditCost             :: BE.Editor T.Text Name
        , purchaseCreationEditFocusRing        :: BF.FocusRing Name
        }
    | RecipeSelectionScreen
        { recipeUIList :: BL.List Name Types.Recipe
        }
    | RecipeDetailScreen
        { recipeInstructions   :: Types.Recipe
        , recipeGlass          :: Maybe Types.Glass
        , recipeIngredientList :: [Types.IngredientListItem]
        , recipePreviousScreen :: CyanideScreen
        }
    | RecipeInputScreen
        { recipeInputName           :: BE.Editor T.Text Name
        , recipeInputGlass          :: BL.List Name Types.Glass
        , recipeInputIngredientList :: BL.List Name Types.IngredientListItem
        , recipeInputInstructions   :: T.Text
        , recipeInputRecipeFor      :: Maybe Types.Ingredient
        , recipeInputBeingModified  :: Maybe Types.Recipe
        , recipeInputFocusRing      :: BF.FocusRing Name
        , recipeInputPreviousScreen :: CyanideScreen
        }
    | RecipeInputIngredientScreen
        { recipeInputIngredientRecipeName    :: T.Text
        , recipeInputIngredientAmount        :: BE.Editor T.Text Name
        , recipeInputIngredientUnit          :: BE.Editor T.Text Name
        , recipeInputIngredientFilter        :: BE.Editor T.Text Name
        , recipeInputIngredientListOrig      :: [RecipeInputIngrListItem]
        , recipeInputIngredientOptionsList   :: BL.List Name RecipeInputIngrListItem
        , recipeInputIngredientFocusRing     :: BF.FocusRing Name
        , recipeInputIngredientPrevScreen    :: Maybe Types.IngredientListItem -> CyanideScreen
        }
    | IngredientClassSelectionScreen
        { ingredientClassUIList :: BL.List Name Types.IngredientClass
        }
    | IngredientClassDeletionScreen
        { ingredientClassDeletionList :: BL.List Name Types.IngredientClass
        }
    | IngredientClassInputScreen
        { ingredientClassCreationName           :: BE.Editor T.Text Name
        , ingredientClassBeingEdited            :: Maybe Types.IngredientClass
        , ingredientClassCreationPreviousScreen :: BL.List Name Types.IngredientClass
        }

data RecipeInputIngrListItem = IngredientListItem Int T.Text
                             | IngredientClassListItem Int T.Text
