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
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Purchases as Purchases
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres
import Cyanide.UI.Util

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn (IngredientSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn MainSelectionScreen

        Vty.EvKey Vty.KEnter [] -> do
            let Just (_,ingr) = BL.listSelectedElement l
            purchases <- liftIO $ Purchases.getPurchasesForIngredient conn ingr
            recipes1 <- liftIO $ Recipes.getRecipesUsingIngredient conn ingr
            recipes2 <- liftIO $ Recipes.getRecipesUsingIngredientClass conn (Types.ingredientClass ingr)
            recipeForIngr <- liftIO $ Recipes.getRecipeForIngredient conn ingr
            B.continue $ CyanideState conn $ IngredientDetailScreen
                ingr
                (BL.list IngredientDetail.purchasesListName (V.fromList purchases) 1)
                (BL.list IngredientDetail.recipesListName (V.fromList (recipes1++recipes2)) 1)
                recipeForIngr
                l
                (BF.focusRing ["IngredientDetailPurchases", "IngredientDetailRecipes"])

        --Vty.EvKey (Vty.KChar 'd') [] ->
        --    B.continue $ CyanideState conn (IngredientDeletionScreen l)

--        Vty.EvKey (Vty.KChar 'n') [] ->
--            B.continue $ CyanideState conn (IngredientCreationScreen (BE.editorText "IngredientCreationScreen" (Just 1) "") l)

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ CyanideState conn (IngredientSelectionScreen newList)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (IngredientSelectionScreen l)) = [ui]
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
listDrawElement sel (Types.Ingredient _ n c a u) =
    BC.hCenter $ B.hBox [ formatText JustifyLeft 24 n
                        , formatText JustifyLeft 12 c
                        , formatText JustifyRight 3 (T.pack $ show a)
                        , B.txt " "
                        , formatText JustifyLeft 12 u
                        ]
