{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.State where

import qualified Data.Text as T
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF

import qualified Cyanide.Config as Config
import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Types as Types

-- | Named resources
type Name = T.Text

data CyanideState = CyanideState
                        { stateDBConn :: Postgres.DBConn
                        , stateConfig :: Config.Config
                        , stateScreen :: CyanideScreen
                        }

data CyanideScreen
    = MainSelectionScreen
    | ErrorScreen
        { errorMessage :: T.Text
        , errorPreviousScreen :: CyanideScreen
        }
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
        , ingredientListOrig      :: [Types.Ingredient]
        , ingredientListSearch    :: BE.Editor T.Text Name
        , ingredientListFocusRing :: BF.FocusRing Name
        }
    | IngredientInputScreen
        { ingredientInputName           :: BE.Editor T.Text Name
        , ingredientInputClass          :: BL.List Name (Maybe Types.IngredientClass)
        , ingredientInputFocusRing      :: BF.FocusRing Name
        , ingredientInputNotForRecipes  :: Bool
        , ingredientInputBeingModified  :: Maybe Types.Ingredient
        , ingredientInputPreviousScreen :: Maybe (Types.Ingredient,Maybe Types.IngredientClass) -> IO CyanideScreen
        }
    | IngredientDetailScreen
        { ingredient                     :: Types.Ingredient
        , ingredientPurchases            :: BL.List Name Types.Purchase
        , ingredientUsedIn               :: BL.List Name Types.Recipe
        , ingredientRecipe               :: Maybe Types.Recipe
        , ingredientFocusRing            :: BF.FocusRing Name
        , ingredientDetailPreviousScreen :: Maybe Types.Ingredient -> CyanideScreen
        }
    | IngredientDeletionScreen
        { ingredientDeletionIngredient           :: Types.Ingredient
        , ingredientDeletionUsedIn               :: [Types.Recipe]
        , ingredientDeletionRecipe               :: Maybe Types.Recipe
        , ingredientDeletionDetailPreviousScreen :: Bool -> CyanideScreen
        }
    | PurchaseDeletionScreen
        { purchaseDeletionPurchase             :: Types.Purchase
        , purchaseDeletionIngredient           :: Types.Ingredient
        , purchaseDeletionDetailPreviousScreen :: Bool -> CyanideScreen
        }
    | PurchaseCreationScreen
        { purchaseCreationIngredient     :: Types.Ingredient
        , purchaseCreationEditLocation   :: BE.Editor T.Text Name
        , purchaseCreationEditCost       :: BE.Editor T.Text Name
        , purchaseCreationEditAmount     :: BE.Editor T.Text Name
        , purchaseCreationEditUnit       :: BE.Editor T.Text Name
        , purchaseCreationEditFocusRing  :: BF.FocusRing Name
        , purchaseCreationPreviousScreen :: Maybe (Types.Ingredient,Types.Purchase) -> CyanideScreen
        }
    | RecipeSelectionScreen
        { recipeList                 :: BL.List Name Types.Recipe
        , recipeListOrig             :: [Types.Recipe]
        , recipeListRecipesToICs     :: [(Int,Int)]
        , recipeListRecipesToGlasses :: [(Int,Int)]
        , recipeListRecipesToAvail   :: [(Int,Bool)]
        , recipeListSearch           :: BE.Editor T.Text Name
        , recipeListFilter           :: RecipeListFilter
        , recipeListFocusRing        :: BF.FocusRing Name
        }
    | RecipeSelectionFilterScreen
        { recipeFilter               :: RecipeListFilter
        , recipeFilterFocusRing      :: BF.FocusRing Name
        , recipeFilterPreviousScreen :: Maybe RecipeListFilter -> IO CyanideScreen
        }
    | RecipeDetailScreen
        { recipeInstructions   :: Types.Recipe
        , recipeGlass          :: Maybe Types.Glass
        , recipeIngredientList :: [Types.IngredientListItem]
        , recipePreviousScreen :: Maybe (Types.Recipe,Maybe Types.Glass,[Types.IngredientListItem]) -> IO CyanideScreen
        }
    | RecipeDeletionScreen
        { recipeDeletionRecipe         :: Types.Recipe
        , recipeDeletionPreviousScreen :: Bool -> IO CyanideScreen
        }
    | RecipeInputScreen
        { recipeInputName           :: BE.Editor T.Text Name
        , recipeInputGarnish        :: BE.Editor T.Text Name
        , recipeInputGlass          :: BL.List Name (Maybe Types.Glass)
        , recipeInputIngredientList :: BL.List Name Types.IngredientListItem
        , recipeInputInstructions   :: T.Text
        , recipeInputRecipeFor      :: Maybe Types.Ingredient
        , recipeInputBeingModified  :: Maybe Types.Recipe
        , recipeInputFocusRing      :: BF.FocusRing Name
        , recipeInputPreviousScreen :: Maybe (Types.Recipe,Maybe Types.Glass,[Types.IngredientListItem]) -> IO CyanideScreen
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

getListItemName :: RecipeInputIngrListItem -> T.Text
getListItemName (IngredientListItem _ n) = n
getListItemName (IngredientClassListItem _ n) = n

data RecipeListFilter = RecipeListFilter
        { onlyWithAvailIngredients :: Bool
        , limitToIngredientClass   :: BL.List Name (Maybe Types.IngredientClass)
        , limitToGlass             :: BL.List Name (Maybe Types.Glass)
        }
