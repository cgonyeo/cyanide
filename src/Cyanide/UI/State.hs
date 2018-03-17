{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.State where

import qualified Data.Text as T
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF

import qualified Cyanide.Config as Config
import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Types as Types

-- | Named resources
type Name = T.Text

-- The data type that holds all app-wide state. This is handed to a given
-- screen's handleEvent function for modification based on an event, and to a
-- given screen's drawUI function for conversion into a brick (the library)
-- widget.
data CyanideState = CyanideState
                        { stateDBConn :: Postgres.DBConn
                        , stateState  :: GlobalState
                        , stateScreen :: CyanideScreen
                        }

-- GlobalState holds top-level state not specific to any screen
data GlobalState = GlobalState
                    { stateConfig :: Config.Config
                    }

-- CyanideScreen denotes which screen is active, and holds whatever state is
-- needed for that screen.
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
        { ingredientSelectionList           :: BL.List Name Types.Ingredient
        , ingredientListOrig                :: [Types.Ingredient]
        , ingredientListFilterNotForRecipes :: Bool
        , ingredientListSearch              :: BE.Editor T.Text Name
        , ingredientListFocusRing           :: BF.FocusRing Name
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
        { recipeInputIngredientAmount        :: BE.Editor T.Text Name
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
    deriving(Eq)

getListItemName :: RecipeInputIngrListItem -> T.Text
getListItemName (IngredientListItem _ n) = n
getListItemName (IngredientClassListItem _ n) = n

data RecipeListFilter = RecipeListFilter
        { onlyWithAvailIngredients :: Bool
        , limitToIngredientClass   :: BL.List Name (Maybe Types.IngredientClass)
        , limitToGlass             :: BL.List Name (Maybe Types.Glass)
        }

getBreadcrumbsForScreen :: CyanideScreen -> B.Widget Name
getBreadcrumbsForScreen MainSelectionScreen                         = B.emptyWidget
getBreadcrumbsForScreen (ErrorScreen _ _)                           = B.emptyWidget
getBreadcrumbsForScreen (GlassSelectionScreen _)                    = B.txt "Home > Glasses"
getBreadcrumbsForScreen (GlassDeletionScreen _)                     = B.txt "Home > Glasses > Deletion"
getBreadcrumbsForScreen (GlassInputScreen _ _ _)                    = B.emptyWidget
getBreadcrumbsForScreen (RecipeSelectionScreen _ _ _ _ _ _ _ _)     = B.txt "Home > Recipes"
getBreadcrumbsForScreen (RecipeSelectionFilterScreen _ _ _)         = B.txt "Home > Recipes > Filter"
getBreadcrumbsForScreen (RecipeDetailScreen _ _ _ _)                = B.emptyWidget
getBreadcrumbsForScreen (RecipeInputScreen _ _ _ _ _ _ _ _ _)       = B.emptyWidget
getBreadcrumbsForScreen (RecipeInputIngredientScreen _ _ _ _ _ _ _) = B.emptyWidget
getBreadcrumbsForScreen (RecipeDeletionScreen _ _)                  = B.emptyWidget
getBreadcrumbsForScreen (IngredientSelectionScreen _ _ _ _ _)         = B.txt "Home > Ingredients"
getBreadcrumbsForScreen (IngredientInputScreen _ _ _ _ _ _)         = B.emptyWidget
getBreadcrumbsForScreen (IngredientDetailScreen _ _ _ _ _ _)        = B.txt "Home > Ingredients > Detail"
getBreadcrumbsForScreen (IngredientDeletionScreen _ _ _ _)          = B.txt "Home > Ingredients > Delete"
getBreadcrumbsForScreen (PurchaseDeletionScreen _ _ _)              = B.txt "Home > Ingredients > Detail > Delete Purchase"
getBreadcrumbsForScreen (PurchaseCreationScreen _ _ _ _ _ _ _)      = B.txt "Home > Ingredients > Detail > Add Purchase"
getBreadcrumbsForScreen (IngredientClassSelectionScreen _)          = B.txt "Home > Ingredient Classes"
getBreadcrumbsForScreen (IngredientClassDeletionScreen _)           = B.txt "Home > Ingredient Classes > Delete"
getBreadcrumbsForScreen (IngredientClassInputScreen _ _ _)          = B.emptyWidget
