{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientDetailScreen where

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
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres
import Cyanide.UI.Util
import qualified Cyanide.UI.PurchaseCreationScreen as PurchaseCreationScreen

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

purchasesListName :: Name
purchasesListName = "IngredientDetailPurchases"

recipesListName :: Name
recipesListName = "IngredientDetailRecipes"

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(IngredientDetailScreen i ps rs mr l f)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn $ IngredientSelectionScreen l

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ CyanideState conn $ IngredientDetailScreen i ps rs mr l newFocus

        Vty.EvKey (Vty.KChar 'p') [] ->
            adjustIngredientAmount (Types.amount i + 1)

        Vty.EvKey (Vty.KChar 'm') [] ->
            if Types.amount i > 0
                then adjustIngredientAmount (Types.amount i - 1)
                else B.continue s

        Vty.EvKey (Vty.KChar 'r') [] ->
            case mr of
                Just r -> do
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ CyanideState conn $ RecipeDetailScreen r Nothing ingrs scr
                Nothing -> B.continue s

        Vty.EvKey Vty.KEnter [] ->
            case (BF.focusGetCurrent f,BL.listSelectedElement rs) of
                (Just "IngredientDetailRecipes",Just (_,r)) -> do
                    glass <- liftIO $ Recipes.getGlassForRecipe conn r
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ CyanideState conn $ RecipeDetailScreen r glass ingrs scr
                (_,_) -> B.continue s

        Vty.EvKey (Vty.KChar 'd') [] ->
            if BF.focusGetCurrent f == Just purchasesListName
                then B.continue $ CyanideState conn (PurchaseDeletionScreen i ps rs mr l f)
                else B.continue s

        Vty.EvKey (Vty.KChar 'n') [] ->
            if BF.focusGetCurrent f == Just purchasesListName
                then let locationEdit = BE.editorText PurchaseCreationScreen.locationEditorName (Just 1) ""
                         costEdit = BE.editorText PurchaseCreationScreen.costEditorName (Just 1) ""
                         fRing = BF.focusRing [PurchaseCreationScreen.locationEditorName, PurchaseCreationScreen.costEditorName]
                     in B.continue $ CyanideState conn (PurchaseCreationScreen i ps rs mr l f locationEdit costEdit fRing)
                else B.continue s

        ev -> if BF.focusGetCurrent (f) == Just purchasesListName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev ps
                    B.continue $ CyanideState conn (IngredientDetailScreen i newList rs mr l f)
              else if BF.focusGetCurrent (f) == Just recipesListName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev rs
                    B.continue $ CyanideState conn (IngredientDetailScreen i ps newList mr l f)
              else B.continue s
    where adjustIngredientAmount :: Int -> B.EventM Name (B.Next CyanideState)
          adjustIngredientAmount newAmount = do
              liftIO $ Ingredients.updateIngredientAmount conn (i,newAmount)
              let newIngredient = i { Types.amount = newAmount }
              B.continue $ CyanideState conn (IngredientDetailScreen newIngredient ps rs mr l f)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (IngredientDetailScreen ing pl rl mr _ f)) = [ui]
    where l1 = BF.withFocusRing f (BL.renderList listDrawPurchase) pl
          l2 = BF.withFocusRing f (BL.renderList listDrawRecipe) rl

          instructions =
              B.padLeft (B.Pad 16) $ 
              B.hBox [ B.vBox $ case mr of
                                Nothing -> [ B.txt "Tab - Change focus"
                                           , B.txt "  p - Plus 1 to amount"
                                           , B.txt "  m - Minus 1 to amount"
                                           , B.txt "Esc - Previous screen"
                                           ]
                                Just _ -> [ B.txt "Tab - Change focus"
                                          , B.txt "  r - View recipe"
                                          , B.txt "  p - Plus 1 to amount"
                                          , B.txt "  m - Minus 1 to amount"
                                          , B.txt "Esc - Previous screen"
                                          ]
                     , B.padLeft (B.Pad 2) $ B.vBox $ 
                        if BF.focusGetCurrent (f) == Just purchasesListName then
                            [ B.txt "n - New purchase"
                            , B.txt "d - Delete purchase"
                            ]
                        else if BF.focusGetCurrent (f) == Just recipesListName then
                            [ B.txt "Enter - See recipe"
                            ]
                        else [ ]
                     ]

          purchases = pl^.(BL.listElementsL)
          
          avgCost = let prices = V.map Types.price purchases
                    in sum prices `div` length prices

          ingredientInfoLabelSize = if length purchases > 0 then 8 else 6

          ingredientInfo =
            B.vBox $ [ addRow ingredientInfoLabelSize "Name" [Types.ingredientName ing]
                     , addRow ingredientInfoLabelSize "Type" [Types.ingredientClass ing]
                     , addRow ingredientInfoLabelSize "Amount" [(T.pack $ show $ Types.amount ing) +++ Types.unit ing]
                     ] ++ if length purchases > 0
                            then [ addRow ingredientInfoLabelSize "Avg Cost" [formatMoney avgCost ] ]
                            else [ ]
    
          -- TODO: alternate UI for ingredients not used in recipes
          ui = BC.center
            $ B.hLimit 80
            $ B.vLimit 25 $ B.vBox $ 
            [ B.hBox
                [ B.vBox
                    [ BB.borderWithLabel (B.txt "Ingredient Info")
                        $ BC.hCenter
                        $ B.padAll 1 $ ingredientInfo
                    , BB.borderWithLabel (B.txt "Purchases") l1
                    ]
                , BB.borderWithLabel (B.txt "Ingredient used in") l2
                ]
            , instructions
            ]

listDrawRecipe :: Bool -> Types.Recipe -> B.Widget Name
listDrawRecipe sel (Types.Recipe _ n _) = B.txt n

listDrawPurchase :: Bool -> Types.Purchase -> B.Widget Name
listDrawPurchase sel (Types.Purchase t l p) =
    B.padLeft (B.Pad 1) $ B.padRight (B.Pad 1)
        $ B.hBox [ formatText JustifyLeft 11 (T.pack $ show t)
                 , formatText JustifyLeft 17 l
                 , formatText JustifyRight 8 $ formatMoney p
                 ]
