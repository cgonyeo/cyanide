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
import qualified Data.List as L

import Cyanide.UI.State
import qualified Cyanide.UI.RecipeInputScreen as RecipeInput
import qualified Cyanide.UI.IngredientInputScreen as IngredientInput
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Purchases as Purchases
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

newIngredientDetailScreen :: Postgres.DBConn -> Types.Ingredient -> (Maybe Types.Ingredient -> CyanideScreen) -> IO CyanideScreen
newIngredientDetailScreen conn ingr prev = do
    purchases <- Purchases.getPurchasesForIngredient conn ingr
    recipes <- getRecipesUsedIn conn ingr
    recipeForIngr <- Recipes.getRecipeForIngredient conn ingr
    let f = BF.focusRing $ [purchasesListName]
                            ++ if Types.notForRecipes ingr
                                    then []
                                    else [recipesListName]
        f' = if length purchases == 0 then BF.focusNext f else f
        purchasesList = BL.list purchasesListName (V.fromList purchases) 1
        usedInList = BL.list recipesListName (V.fromList recipes) 1
    return $ IngredientDetailScreen ingr purchasesList usedInList recipeForIngr f' prev

getRecipesUsedIn :: Postgres.DBConn -> Types.Ingredient -> IO [Types.Recipe]
getRecipesUsedIn conn ingr = do
    recipes1 <- liftIO $ Recipes.getRecipesUsingIngredient conn ingr
    recipes2 <- case Types.ingredientClass ingr of
                        Just ic -> do
                            rlst <- liftIO $ Recipes.getRecipesUsingIngredientClass conn ic
                            return rlst
                        Nothing -> return []
    return $ L.sortBy (\r1 r2 -> compare (getRecipeName r1) (getRecipeName r2))
                $ recipes1 ++ recipes2
  where getRecipeName :: Types.Recipe -> T.Text
        getRecipeName r = case Types.recipeName r of
                            Left t -> t
                            Right i -> "recipe for " `T.append` Types.ingredientName i

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientDetailScreen i ps rs mr f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = prev (Just i) }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { ingredientFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'd') [Vty.MMeta] -> do
            recipes <- liftIO $ Recipes.getRecipesUsingIngredient conn i
            B.continue $ s { stateScreen = (IngredientDeletionScreen i recipes mr goBack) }
          where goBack False = scr
                goBack True = prev Nothing

        Vty.EvKey (Vty.KChar 'a') [] -> do
            liftIO $ Ingredients.updateIngredientAvailability conn (i,not $ Types.available i)
            let newIngredient = i { Types.available = not $ Types.available i }
            B.continue $ s { stateScreen = scr { ingredient = newIngredient } }

        Vty.EvKey (Vty.KChar 'e') [] -> do
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn
            let selectedIcIndex = getIndex (Types.ingredientClass i) (Just) ics
                selectedIcIndex' = if isJust (Types.ingredientClass i) then selectedIcIndex + 1
                                                                       else selectedIcIndex
                ed = BE.editor IngredientInput.editorName (Just 1) (Types.ingredientName i)
                iclist = BL.listMoveTo selectedIcIndex'
                                $ BL.list IngredientInput.classesName (V.fromList $ [Nothing] ++ map Just ics) 1
                f = BF.focusRing [ IngredientInput.editorName
                                 , IngredientInput.classesName
                                 ]
            B.continue $ s { stateScreen = (IngredientInputScreen ed iclist f (Types.notForRecipes i) (Just i) goBack) }
          where getIndex :: (Eq b) => b -> (a -> b) -> [a] -> Int
                getIndex mustEqual toEqualForm lst =
                      let matches = filter (\x -> snd x == mustEqual) $ zip [0..] (map toEqualForm lst)
                      in case matches of
                          [(i,_)] -> i
                          _ -> 0
                goBack Nothing = return $ scr
                goBack (Just (ingr,_)) = do
                    recipes <- liftIO $ getRecipesUsedIn conn ingr
                    let newIndex = BL.listSelectedElement rs >>= (Just . fst)
                        newList = BL.listReplace (V.fromList recipes) newIndex rs
                    return $ scr { ingredient = ingr
                                 , ingredientUsedIn = newList
                                 }

        Vty.EvKey (Vty.KChar 'r') [] ->
            case mr of
                Just r -> do
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ s { stateScreen = RecipeDetailScreen r Nothing ingrs (return . goBack) }
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
                                   in return $ scr { ingredientUsedIn = newList }
                goBack j (Just _) = do
                    recipes <- liftIO $ getRecipesUsedIn conn i
                    let newList = BL.listReplace (V.fromList recipes) (Just j) rs
                    return $ scr { ingredientUsedIn = newList }
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
                         amountEdit = BE.editorText PurchaseCreationScreen.amountEditorName (Just 1) "750"
                         unitEdit = BE.editorText PurchaseCreationScreen.unitEditorName (Just 1) "mL"
                         fRing = BF.focusRing [ PurchaseCreationScreen.locationEditorName
                                              , PurchaseCreationScreen.costEditorName
                                              , PurchaseCreationScreen.amountEditorName
                                              , PurchaseCreationScreen.unitEditorName
                                              ]
                     in B.continue $ s { stateScreen = (PurchaseCreationScreen i locationEdit costEdit amountEdit unitEdit fRing goBack) }
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
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientDetailScreen ing pl rl mr f prev)) = [ui]
    where l1 = BF.withFocusRing f (BL.renderList (listDrawPurchase notForRecipes)) pl
          l2 = BF.withFocusRing f (BL.renderList listDrawRecipe) rl

          notForRecipes = Types.notForRecipes ing

          instructions =
              B.padLeft (B.Pad 6) $
              B.hBox [ B.vBox [ if notForRecipes then B.emptyWidget else B.txt "  Tab - Change focus"
                              , B.txt "Alt-d - Delete ingredient"
                              , B.txt "  Esc - Previous screen"
                              ]
                     , B.padLeft (B.Pad 2) $ B.vBox
                        [ if isJust mr
                             then B.txt "r - View recipe"
                             else B.txt "r - Create recipe"
                        , B.txt "a - Toggle availability"
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

          avgCost = Units.calculateAvgPricePerOz (V.toList purchases)

          ingredientInfoLabelSize = 9

          handleIc :: Maybe Types.IngredientClass -> B.Widget Name
          handleIc (Just ic) = addRow ingredientInfoLabelSize "Type" [B.txt $ Types.ingredientClassName ic]
          handleIc Nothing = B.emptyWidget

          ingredientInfo =
            B.vBox $ [ addRow ingredientInfoLabelSize "Name" [B.txt $ Types.ingredientName ing]
                     , handleIc (Types.ingredientClass ing)
                     , addRow ingredientInfoLabelSize "Available" [B.txt $ T.pack $ show $ Types.available ing]
                     ] ++ case avgCost of
                            Nothing -> []
                            Just cost -> [ addRow ingredientInfoLabelSize "Avg Cost" [B.txt $ formatMoney cost `T.append` "/oz" ] ]


          ui = B.vBox $
            [ B.hBox
                [ B.vBox
                    [ BB.borderWithLabel (B.txt "Ingredient Info")
                        $ B.padAll 1
                        $ B.vBox $ [ BC.hCenter ingredientInfo
                               ] ++ case mr of
                                        Nothing -> []
                                        Just _ -> [ B.txt " ", BC.hCenter $ B.txt "This ingredient has a recipe" ]
                                 ++ if Types.notForRecipes ing
                                      then [ B.txt " ", BC.hCenter $ B.txt "This should not be used in cocktails" ]
                                      else []
                    , BB.borderWithLabel (B.txt "Purchases") l1
                    ]
                , if notForRecipes
                    then B.emptyWidget
                    else B.hLimit 30 $ BB.borderWithLabel (B.txt "Ingredient used in") l2
                ]
            , instructions
            ]

listDrawRecipe :: Bool -> Types.Recipe -> B.Widget Name
listDrawRecipe sel (Types.Recipe _ (Left n) _ _) = B.txt n
listDrawRecipe sel (Types.Recipe _ (Right i) _ _) = B.txt $ "recipe for " `T.append` Types.ingredientName i

listDrawPurchase :: Bool -> Bool -> Types.Purchase -> B.Widget Name
listDrawPurchase notForRecipes sel (Types.Purchase t l p a u) =
    BC.hCenter
    $ B.padLeft (B.Pad 1) $ B.padRight (B.Pad 1)
        $ B.hBox [ formatText JustifyLeft 11 (T.pack $ show t)
                 , formatText JustifyLeft (if notForRecipes then 30 else 15) l
                 , formatText JustifyRight 8 $ formatMoney p
                 , formatText JustifyRight 12 ((T.pack $ show a) `T.append` " " `T.append` u)
                 ]
