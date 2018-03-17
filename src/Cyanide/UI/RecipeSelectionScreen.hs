{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeSelectionScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.List as L
import System.Random
import qualified Data.Foldable as F

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.UI.RecipeInputScreen as RecipeInput
import qualified Cyanide.UI.RecipeSelectionFilterScreen as RecipeSelectionFilter
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.Postgres as Postgres

recipesName :: Name
recipesName = "RecipeSelectionRecipeList"

searchName :: Name
searchName = "RecipeSelectionSearchEditor"

newRecipeSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
newRecipeSelectionScreen conn = do
    recipes <- Recipes.getRecipes conn
    ingredientClasses <- IngredientClasses.getIngredientClasses conn
    glasses <- Glasses.getGlasses conn
    recipesToICs <- Recipes.getRecipesToIngredientClasses conn
    recipesToGlasses <- Recipes.getRecipesToGlasses conn
    recipesToAvail <- Recipes.getRecipesIngredientAvailability conn
    let rList = BL.list recipesName (V.fromList recipes) 1
        rListSearch = BE.editorText searchName (Just 1) ""
        icList = BL.list RecipeSelectionFilter.ingredientClassesName (V.fromList $ [Nothing] ++ map Just ingredientClasses) 1
        gList = BL.list RecipeSelectionFilter.glassesName (V.fromList $ [Nothing] ++ map Just glasses) 1
        f = BF.focusRing [ recipesName
                         , searchName
                         ]
    return $ RecipeSelectionScreen rList recipes recipesToICs recipesToGlasses recipesToAvail rListSearch (RecipeListFilter False icList gList) f

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(RecipeSelectionScreen l orig toICs toGs toAs se fltr f)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { recipeListFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'f') [Vty.MMeta] ->
            let focus = BF.focusRing [ RecipeSelectionFilter.ingredientClassesName
                                     , RecipeSelectionFilter.glassesName
                                     ]
            in B.continue $ s { stateScreen = RecipeSelectionFilterScreen fltr focus goBack }
          where goBack :: Maybe RecipeListFilter -> IO CyanideScreen
                goBack Nothing = return $ scr
                goBack (Just rlf@(RecipeListFilter avail icl gl)) = do
                    let Just (_,ic) = BL.listSelectedElement icl
                        Just (_,g) = BL.listSelectedElement gl
                        searchText = getEditorLine se
                        filteredRecipes = genNewList orig avail ic g searchText
                        newList = BL.list recipesName (V.fromList filteredRecipes) 1
                    return $ scr { recipeList = newList
                                 , recipeListFilter = rlf
                                 }

        Vty.EvKey (Vty.KChar 'r') [Vty.MMeta] -> do
            num <- liftIO $ randomRIO (0, length l - 1)
            let r = F.toList l !! num
            glass <- liftIO $ Recipes.getGlassForRecipe conn r
            ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
            B.continue $ s { stateScreen = RecipeDetailScreen r glass ingrs (goBack num) }
          where goBack j Nothing = let newList = BL.listRemove j l
                                       Just (_,rec) = BL.listSelectedElement l
                                       newOrig = filter (/=rec) orig
                                   in return $ scr { recipeList = newList
                                                   , recipeListOrig = newOrig
                                                   }
                goBack j (Just (r,_,_)) = let newList = BL.listInsert j r $ BL.listRemove j l
                                              newOrig = map (\r' -> if Types.recipeId r == Types.recipeId r' then r else r') orig
                                          in return $ scr { recipeList = newList
                                                          , recipeListOrig = newOrig
                                                          }


        Vty.EvKey (Vty.KChar 'n') [Vty.MMeta] -> do
            newScr <- liftIO $ RecipeInput.newRecipeInputScreen conn Nothing [] Nothing Nothing goBack
            B.continue $ s { stateScreen = newScr }
          where goBack Nothing = return scr
                goBack (Just _) = newRecipeSelectionScreen conn

        Vty.EvKey (Vty.KChar '/') [] ->
            if BF.focusGetCurrent (f) == Just recipesName
                then let newFocus = BF.focusNext f
                     in B.continue $ s { stateScreen = scr { recipeListFocusRing = newFocus } }
                else B.continue s


        Vty.EvKey (Vty.KEnter) [] ->
            if BF.focusGetCurrent (f) == Just recipesName then
                case (BL.listSelectedElement l) of
                    Nothing -> B.continue s
                    Just (j,r) -> do
                        glass <- liftIO $ Recipes.getGlassForRecipe conn r
                        ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                        B.continue $ s { stateScreen = RecipeDetailScreen r glass ingrs (goBack j) }
            else if BF.focusGetCurrent (f) == Just searchName then
                let newFocus = BF.focusNext f
                in B.continue $ s { stateScreen = scr { recipeListFocusRing = newFocus } }
            else B.continue s
          where goBack j Nothing = let newList = BL.listRemove j l
                                       Just (_,rec) = BL.listSelectedElement l
                                       newOrig = filter (/=rec) orig
                                   in return $ scr { recipeList = newList
                                                   , recipeListOrig = newOrig
                                                   }
                goBack _ (Just (r,_,_)) = let newList = BL.listModify (\_ -> r) l
                                              newOrig = map (\r' -> if Types.recipeId r == Types.recipeId r' then r else r') orig
                                          in return $ scr { recipeList = newList
                                                          , recipeListOrig = newOrig
                                                          }

        ev -> if BF.focusGetCurrent (f) == Just recipesName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev l
                    B.continue $ s { stateScreen = scr { recipeList = newList } }
              else if BF.focusGetCurrent (f) == Just searchName then do
                    newEdit <- BE.handleEditorEvent ev se
                    let RecipeListFilter avail icl gl = fltr
                        Just (_,ic) = BL.listSelectedElement icl
                        Just (_,g) = BL.listSelectedElement gl
                        searchText = getEditorLine newEdit
                        filteredRecipes = genNewList orig avail ic g searchText
                        newList = BL.list recipesName (V.fromList filteredRecipes) 1
                    B.continue $ s { stateScreen = scr { recipeListSearch = newEdit
                                                       , recipeList = newList
                                                       } }
              else B.continue s
  where genNewList :: [Types.Recipe] -> Bool -> Maybe Types.IngredientClass -> Maybe Types.Glass -> Maybe T.Text -> [Types.Recipe]
        genNewList recipes a mic mg search =
            let recipes1 = filterOutIc mic recipes
                recipes2 = filterOutG mg recipes1
                recipes3 = filterOutA a recipes2
                recipes4 = filterOutSearch search recipes3
            in recipes4

        lookForId :: Int -> Int -> [(Int,Int)] -> Bool
        lookForId _ _ [] = False
        lookForId id1 id2 ((id3,id4):t) = if id1 == id3 && id2 == id4
                                                then True
                                                else lookForId id1 id2 t

        filterOutIc :: Maybe Types.IngredientClass -> [Types.Recipe] -> [Types.Recipe]
        filterOutIc Nothing rs = rs
        filterOutIc (Just (Types.IngredientClass icId _)) rs =
            filter (\(Types.Recipe recId _ _ _) -> elem (recId,icId) toICs) rs

        filterOutG :: Maybe Types.Glass -> [Types.Recipe] -> [Types.Recipe]
        filterOutG Nothing rs = rs
        filterOutG (Just (Types.Glass gId _)) rs =
            filter (\(Types.Recipe recId _ _ _) -> lookForId recId gId toGs) rs

        filterOutA :: Bool -> [Types.Recipe] -> [Types.Recipe]
        filterOutA False rs = rs
        filterOutA True rs = filter (\(Types.Recipe recId _ _ _) ->
                                case lookup recId toAs of
                                        Just True -> True
                                        _ -> False) rs

        filterOutSearch :: Maybe T.Text -> [Types.Recipe] -> [Types.Recipe]
        filterOutSearch (Just search) rs = filter (filterFunc search) rs
        filterOutSearch Nothing rs = rs

        filterFunc filterText (Types.Recipe _ (Left n) _ _) = L.isInfixOf (T.unpack $ T.toLower filterText) (T.unpack $ T.toLower n)
        filterFunc _ _ = False
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (RecipeSelectionScreen l orig _ _ _ se fltr f)) = [ui]
    where editor = BF.withFocusRing f (BE.renderEditor drawEdit) se
          recipeList = BF.withFocusRing f (BL.renderList listDrawRecipe) l
          ui = B.vBox
                    [ B.vBox [ BB.borderWithLabel (B.txt "Search") editor
                             , BB.borderWithLabel (B.txt "Recipes") recipeList
                             ]
                    , renderInstructions [ ("Alt-f","Filter list")
                                         , ("Alt-n","New recipe")
                                         , ("Alt-r","Random recipe")
                                         , ("Esc","Previous screen")
                                         ]
                    ]

listDrawRecipe :: Bool -> Types.Recipe -> B.Widget Name
listDrawRecipe sel (Types.Recipe _ (Left n) _ _) = BC.hCenter $ B.str $ T.unpack n
listDrawRecipe sel (Types.Recipe _ (Right i) _ _) = BC.hCenter $ B.str $ T.unpack $ "recipe for " `T.append` Types.ingredientName i

drawEdit = B.txt . T.unlines
