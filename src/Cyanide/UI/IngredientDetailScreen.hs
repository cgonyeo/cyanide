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
import Data.Maybe

import Cyanide.UI.State
import qualified Cyanide.UI.RecipeInputScreen as RecipeInput
import qualified Cyanide.UI.IngredientInputScreen as IngredientInput
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Units as Units
import Cyanide.UI.Util
import qualified Cyanide.UI.PurchaseCreationScreen as PurchaseCreationScreen

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

purchasesListName :: Name
purchasesListName = "IngredientDetailPurchases"

recipesListName :: Name
recipesListName = "IngredientDetailRecipes"

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientDetailScreen i mic ps rs mr f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = prev (Just i) }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { ingredientFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'p') [] ->
            adjustIngredientAmount (Types.amount i + 1)

        Vty.EvKey (Vty.KChar 'm') [] ->
            if Types.amount i > 0
                then adjustIngredientAmount (Types.amount i - 1)
                else B.continue s

        Vty.EvKey (Vty.KChar 'd') [Vty.MMeta] -> do
            recipes <- liftIO $ Recipes.getRecipesUsingIngredient conn i
            B.continue $ s { stateScreen = (IngredientDeletionScreen i recipes mr goBack) }
          where goBack False = scr
                goBack True = prev Nothing

        Vty.EvKey (Vty.KChar 'e') [] -> do
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn
            let selectedIcIndex = getIndex (Types.ingredientClass i) (Just . Types.ingredientClassId) ics
                selectedIcIndex' = if isJust (Types.ingredientClass i) then selectedIcIndex + 1
                                                                       else selectedIcIndex
                selectedUnitIndex = getIndex (Types.unit i) id Units.ingredientUnits
                ed = BE.editor IngredientInput.editorName (Just 1) (Types.ingredientName i)
                iclist = BL.listMoveTo selectedIcIndex'
                                $ BL.list IngredientInput.classesName (V.fromList $ [Nothing] ++ map Just ics) 1
                ulist = BL.listMoveTo selectedUnitIndex
                                $ BL.list IngredientInput.unitsName (V.fromList Units.ingredientUnits) 1
                f = BF.focusRing [ IngredientInput.editorName
                                 , IngredientInput.classesName
                                 , IngredientInput.unitsName
                                 ]
            B.continue $ s { stateScreen = (IngredientInputScreen ed iclist ulist f (Types.notForRecipes i) (Just i) goBack) }
          where getIndex :: (Eq b) => b -> (a -> b) -> [a] -> Int
                getIndex mustEqual toEqualForm lst =
                      let matches = filter (\x -> snd x == mustEqual) $ zip [0..] (map toEqualForm lst)
                      in case matches of
                          [(i,_)] -> i
                          _ -> 0
                goBack Nothing = scr
                goBack (Just (ingr,mic)) = scr { ingredient = ingr
                                             , ingredientClass = mic
                                             }

        Vty.EvKey (Vty.KChar 'r') [] ->
            case mr of
                Just r -> do
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ s { stateScreen = RecipeDetailScreen r Nothing ingrs goBack }
                Nothing -> do
                    newScr <- liftIO $ RecipeInput.newRecipeInputScreen conn Nothing [] (Just i) Nothing (return . goBack)
                    B.continue $ s { stateScreen = newScr }
          where goBack Nothing = scr { ingredientRecipe = Nothing }
                goBack (Just (r,_,_)) = scr { ingredientRecipe = Just r }

        Vty.EvKey Vty.KEnter [] ->
            case (BF.focusGetCurrent f,BL.listSelectedElement rs) of
                (Just "IngredientDetailRecipes",Just (j,r)) -> do
                    glass <- liftIO $ Recipes.getGlassForRecipe conn r
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ s { stateScreen = RecipeDetailScreen r glass ingrs (goBack j) }
                (_,_) -> B.continue s
          where goBack j Nothing = let newList = BL.listRemove j rs
                                   in scr { ingredientUsedIn = newList }
                goBack _ (Just (r,_,_)) = let newList = BL.listModify (\_ -> r) rs
                                          in scr { ingredientUsedIn = newList }

        Vty.EvKey (Vty.KChar 'd') [] ->
            if BF.focusGetCurrent f == Just purchasesListName
                then case BL.listSelectedElement ps of
                        Nothing -> B.continue s
                        Just (j,purchase) -> B.continue $ s { stateScreen = (PurchaseDeletionScreen purchase i (goBack j)) }
                else B.continue s
          where goBack n False = scr
                goBack n True =
                    let newList = BL.listRemove n ps
                    in scr { ingredientPurchases = newList }

        Vty.EvKey (Vty.KChar 'n') [] ->
            if BF.focusGetCurrent f == Just purchasesListName
                then let locationEdit = BE.editorText PurchaseCreationScreen.locationEditorName (Just 1) ""
                         costEdit = BE.editorText PurchaseCreationScreen.costEditorName (Just 1) ""
                         fRing = BF.focusRing [PurchaseCreationScreen.locationEditorName, PurchaseCreationScreen.costEditorName]
                     in B.continue $ s { stateScreen = (PurchaseCreationScreen i locationEdit costEdit fRing goBack) }
                else B.continue s
          where goBack Nothing = scr
                goBack (Just (i,p)) = let newPurchases = BL.listInsert 0 p ps
                                      in scr { ingredientPurchases = newPurchases
                                             , ingredient = i
                                             }

        ev -> if BF.focusGetCurrent (f) == Just purchasesListName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev ps
                    B.continue $ s { stateScreen = scr { ingredientPurchases = newList } }
              else if BF.focusGetCurrent (f) == Just recipesListName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev rs
                    B.continue $ s { stateScreen = scr { ingredientUsedIn = newList } }
              else B.continue s
    where adjustIngredientAmount :: Int -> B.EventM Name (B.Next CyanideState)
          adjustIngredientAmount newAmount = do
              liftIO $ Ingredients.updateIngredientAmount conn (i,newAmount)
              let newIngredient = i { Types.amount = newAmount }
              B.continue $ s { stateScreen = scr { ingredient = newIngredient } }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientDetailScreen ing mic pl rl mr f prev)) = [ui]
    where l1 = BF.withFocusRing f (BL.renderList listDrawPurchase) pl
          l2 = BF.withFocusRing f (BL.renderList listDrawRecipe) rl

          instructions =
              B.padLeft (B.Pad 6) $ 
              B.hBox [ B.vBox [ B.txt "  Tab - Change focus"
                              , if isJust mr
                                   then B.txt "    r - View recipe"
                                   else B.txt "    r - Create recipe"
                              , B.txt "Alt-d - Delete ingredient"
                              , B.txt "  Esc - Previous screen"
                              ]
                     , B.padLeft (B.Pad 2) $ B.vBox
                        [ B.txt "p - Plus 1 to amount"
                        , B.txt "m - Minus 1 to amount"
                        , B.txt "e - Edit ingredient"
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

          handleIc :: Maybe Types.IngredientClass -> B.Widget Name
          handleIc (Just ic) = addRow ingredientInfoLabelSize "Type" [B.txt $ Types.ingredientClassName ic]
          handleIc Nothing = B.emptyWidget

          ingredientInfo =
            B.vBox $ [ addRow ingredientInfoLabelSize "Name" [B.txt $ Types.ingredientName ing]
                     , handleIc mic 
                     , addRow ingredientInfoLabelSize "Amount" [B.txt $ (T.pack $ show $ Types.amount ing) +++ Types.unit ing]
                     ] ++ if length purchases > 0
                            then [ addRow ingredientInfoLabelSize "Avg Cost" [B.txt $ formatMoney avgCost ] ]
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
listDrawRecipe sel (Types.Recipe _ (Left n) _ _) = B.txt n
listDrawRecipe sel (Types.Recipe _ (Right i) _ _) = B.txt $ "recipe for " `T.append` Types.ingredientName i

listDrawPurchase :: Bool -> Types.Purchase -> B.Widget Name
listDrawPurchase sel (Types.Purchase t l p) =
    B.padLeft (B.Pad 1) $ B.padRight (B.Pad 1)
        $ B.hBox [ formatText JustifyLeft 11 (T.pack $ show t)
                 , formatText JustifyLeft 17 l
                 , formatText JustifyRight 8 $ formatMoney p
                 ]
