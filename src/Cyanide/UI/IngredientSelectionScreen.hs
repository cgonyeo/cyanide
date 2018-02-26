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

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey Vty.KEnter [] -> do
            let Just (j,ingr) = BL.listSelectedElement l
            purchases <- liftIO $ Purchases.getPurchasesForIngredient conn ingr
            recipes1 <- liftIO $ Recipes.getRecipesUsingIngredient conn ingr
            (recipes2,mic) <- case Types.ingredientClass ingr of
                                Just icId -> do
                                    rlst <- liftIO $ Recipes.getRecipesUsingIngredientClass conn icId
                                    ic <- liftIO $ IngredientClasses.getIngredientClass conn icId
                                    return (rlst,Just ic)
                                Nothing -> return ([],Nothing)
            recipeForIngr <- liftIO $ Recipes.getRecipeForIngredient conn ingr
            B.continue $ s { stateScreen = IngredientDetailScreen
                                            ingr
                                            mic
                                            (BL.list IngredientDetail.purchasesListName (V.fromList purchases) 1)
                                            (BL.list IngredientDetail.recipesListName (V.fromList (recipes1++recipes2)) 1)
                                            recipeForIngr
                                            (BF.focusRing ["IngredientDetailPurchases", "IngredientDetailRecipes"])
                                            (goBack j) }
          where goBack n (Just i) = let newList = BL.listModify (\_ -> i) l
                                    in scr { ingredientSelectionList = newList }
                goBack n Nothing = let newList = BL.listRemove n l
                                   in scr { ingredientSelectionList = newList }

        Vty.EvKey (Vty.KChar 'n') [] -> do
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn

            let ed = BE.editor IngredientInput.editorName (Just 1) ""
                iclist = BL.list IngredientInput.classesName (V.fromList $ [Nothing] ++ map Just ics) 1
                ulist = BL.list IngredientInput.unitsName (V.fromList Units.ingredientUnits) 1
                f = BF.focusRing [ IngredientInput.editorName
                                 , IngredientInput.classesName
                                 , IngredientInput.unitsName
                                 ]
            B.continue $ s { stateScreen = (IngredientInputScreen ed iclist ulist f False Nothing goBack) }
          where goBack Nothing = scr
                goBack (Just (i,_)) = 
                    let newList = BL.listInsert (length l) i l
                        newList' = BL.listMoveTo (length newList) newList
                    in scr { ingredientSelectionList = newList' }

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ s { stateScreen = (IngredientSelectionScreen newList) }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientSelectionScreen l)) = [ui]
    where box = BB.borderWithLabel (B.txt "Ingredients") $
              BL.renderList listDrawElement True l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter box
                            , renderInstructions [ ("Enter","View details")
                                                 , ("n","New ingredient")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Ingredient -> B.Widget Name
listDrawElement sel (Types.Ingredient _ n _ a u _) =
    BC.hCenter $ B.hBox [ formatText JustifyLeft 45 n
                        , formatText JustifyRight 3 (T.pack $ show a)
                        , B.txt " "
                        , formatText JustifyLeft 18 u
                        ]
