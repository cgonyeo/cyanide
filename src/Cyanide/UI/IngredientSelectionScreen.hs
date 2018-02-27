{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientSelectionScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.List as L

import Cyanide.UI.State
import qualified Cyanide.UI.IngredientDetailScreen as IngredientDetail
import qualified Cyanide.UI.IngredientInputScreen as IngredientInput
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Purchases as Purchases
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Units as Units
import Cyanide.UI.Util

ingredientsName :: Name
ingredientsName = "IngredientSelectionIngredientsList"

searchName :: Name
searchName = "IngredientSelectionSearchEditor"

newIngredientSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
newIngredientSelectionScreen conn = do
    ingredients <- Ingredients.getIngredients conn
    let ingList = BL.list ingredientsName (V.fromList ingredients) 1
        se = BE.editorText searchName (Just 1) ""
        f = BF.focusRing [ ingredientsName, searchName ]
    return $ IngredientSelectionScreen ingList ingredients se f

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientSelectionScreen l orig se f)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar '/') [] ->
            if BF.focusGetCurrent (f) == Just ingredientsName
                then let newFocus = BF.focusNext f
                     in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }
                else B.continue s

        Vty.EvKey Vty.KEnter [] ->
            if BF.focusGetCurrent (f) == Just ingredientsName then do
                let Just (j,ingr) = BL.listSelectedElement l
                purchases <- liftIO $ Purchases.getPurchasesForIngredient conn ingr
                recipes1 <- liftIO $ Recipes.getRecipesUsingIngredient conn ingr
                recipes2 <- case Types.ingredientClass ingr of
                                    Just ic -> do
                                        rlst <- liftIO $ Recipes.getRecipesUsingIngredientClass conn ic
                                        return rlst
                                    Nothing -> return []
                recipeForIngr <- liftIO $ Recipes.getRecipeForIngredient conn ingr
                let f = BF.focusRing $ [IngredientDetail.purchasesListName]
                                        ++ if Types.notForRecipes ingr
                                                then []
                                                else [IngredientDetail.recipesListName]
                B.continue $ s { stateScreen = IngredientDetailScreen
                                                ingr
                                                (BL.list IngredientDetail.purchasesListName (V.fromList purchases) 1)
                                                (BL.list IngredientDetail.recipesListName (V.fromList (recipes1++recipes2)) 1)
                                                recipeForIngr
                                                f
                                                (goBack j) }
            else if BF.focusGetCurrent (f) == Just searchName then
                let newFocus = BF.focusNext f
                in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }
            else B.continue s
          where goBack n (Just i) = let newList = BL.listModify (\_ -> i) l
                                        newOrig = replaceIng orig i
                                    in scr { ingredientSelectionList = newList
                                           , ingredientListOrig = newOrig
                                           }
                goBack n Nothing = let newList = BL.listRemove n l
                                       Just (_,ingr) = BL.listSelectedElement l
                                       newOrig = removeIng orig ingr
                                   in scr { ingredientSelectionList = newList
                                          , ingredientListOrig = newOrig
                                          }

                replaceIng :: [Types.Ingredient] -> Types.Ingredient -> [Types.Ingredient]
                replaceIng [] _ = []
                replaceIng (ing1:t) ing2 =
                    if Types.ingredientId ing1 == Types.ingredientId ing2
                        then ing2 : t
                        else ing1 : replaceIng t ing2

                removeIng :: [Types.Ingredient] -> Types.Ingredient -> [Types.Ingredient]
                removeIng [] _ = []
                removeIng (ing1:t) ing2 =
                    if Types.ingredientId ing1 == Types.ingredientId ing2
                        then t
                        else ing1 : replaceIng t ing2

        Vty.EvKey (Vty.KChar 'n') [Vty.MMeta] -> do
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn

            let ed = BE.editor IngredientInput.editorName (Just 1) ""
                iclist = BL.list IngredientInput.classesName (V.fromList $ [Nothing] ++ map Just ics) 1
                f = BF.focusRing [ IngredientInput.editorName
                                 , IngredientInput.classesName
                                 ]
            B.continue $ s { stateScreen = (IngredientInputScreen ed iclist f False Nothing goBack) }
          where goBack Nothing = scr
                goBack (Just (i,_)) =
                    let newList = BL.listInsert (length l) i l
                        newList' = BL.listMoveTo (length newList) newList
                    in scr { ingredientSelectionList = newList' }

        ev -> if BF.focusGetCurrent (f) == Just ingredientsName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev l
                    B.continue $ s { stateScreen = scr { ingredientSelectionList = newList } }
              else if BF.focusGetCurrent (f) == Just searchName then do
                    newEdit <- BE.handleEditorEvent ev se
                    let filteredRecipes = filterOutSearch (getEditorLine newEdit) orig
                        newList = BL.list ingredientsName (V.fromList filteredRecipes) 1
                    B.continue $ s { stateScreen = scr { ingredientListSearch = newEdit
                                                       , ingredientSelectionList = newList
                                                       } }
              else B.continue s
          where filterOutSearch :: Maybe T.Text -> [Types.Ingredient] -> [Types.Ingredient]
                filterOutSearch (Just search) is = filter (filterFunc search) is
                filterOutSearch Nothing is = is

                filterFunc filterText ing = L.isInfixOf (T.unpack $ T.toLower filterText) (T.unpack $ T.toLower (Types.ingredientName ing))

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientSelectionScreen l _ se f)) = [ui]
    where editor = BF.withFocusRing f (BE.renderEditor drawEdit) se
          ingredientList = BF.withFocusRing f (BL.renderList listDrawElement) l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BB.borderWithLabel (B.txt "Search") editor
                            , BB.borderWithLabel (B.txt "Ingredients") ingredientList
                            , renderInstructions [ ("Enter","View details")
                                                 , ("Alt-n","New ingredient")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Ingredient -> B.Widget Name
listDrawElement sel (Types.Ingredient _ n mic a _) =
    BC.hCenter $ B.hBox [ formatText JustifyLeft 46 n
                        , formatText JustifyLeft 18 $ case mic of
                                                        Just (Types.IngredientClass _ n) -> n
                                                        _ -> ""
                        , formatText JustifyRight 12 (if a then "Available" else "")
                        ]

drawEdit = B.txt . T.unlines
