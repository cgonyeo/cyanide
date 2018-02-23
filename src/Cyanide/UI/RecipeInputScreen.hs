{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeInputScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Focus as BF
import Data.Monoid
import Data.Maybe
import Control.Monad.IO.Class
import System.Environment
import System.Process
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.SHA

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.UI.RecipeInputIngredientScreen as RecipeInputIngredient
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Postgres as Postgres

recipeName :: Name
recipeName = "RecipeCreationName"

garnishName :: Name
garnishName = "RecipeCreationGarnish"

glassName :: Name
glassName = "RecipeCreationGlassList"

ingredientsName :: Name
ingredientsName = "RecipeCreationIngredientList"

newRecipeInputScreen :: Postgres.DBConn -> Maybe Types.Glass -> [Types.IngredientListItem] -> Maybe Types.Ingredient -> Maybe Types.Recipe -> (Maybe (Types.Recipe,Maybe Types.Glass,[Types.IngredientListItem]) -> IO CyanideScreen) -> IO CyanideScreen
newRecipeInputScreen conn mGlass ingrList recipeFor mRecipeBeingModified goBack = do
    glasses <- liftIO $ Glasses.getGlasses conn
    let mGlassList = [Nothing] ++ map Just glasses
        glassIndex = getGlassIndex mGlass (zip [0..] mGlassList)

        (name,garnish,instructions) =
            case mRecipeBeingModified of
                Just (Types.Recipe _ (Left n) g instr) -> (n,g,instr)
                Just (Types.Recipe _ _ g instr) -> ("",g,instr)
                _ -> ("","","")


        nameEd = BE.editor recipeName (Just 1) name
        garnishEd = BE.editor garnishName (Just 1) garnish
        glist = BL.listMoveTo glassIndex $ BL.list glassName (V.fromList mGlassList) 1
        ilist = BL.list ingredientsName (V.fromList ingrList) 1
        f = case recipeFor of
                Just _ -> BF.focusRing [ ingredientsName]
                Nothing -> BF.focusRing [ recipeName
                                        , garnishName
                                        , glassName
                                        , ingredientsName
                                        ]
    return $ RecipeInputScreen nameEd garnishEd glist ilist instructions recipeFor mRecipeBeingModified f goBack

  where getGlassIndex :: Eq a => a -> [(Int,a)] -> Int
        getGlassIndex _ [] = 0
        getGlassIndex g ((i,h):t) = if g == h then i else getGlassIndex g t

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(RecipeInputScreen nameEd garnishEd gl il instr recipeFor mr f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] -> do
            newScr <- liftIO $ prev Nothing
            B.continue $ CyanideState conn newScr

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ CyanideState conn $ scr { recipeInputFocusRing = newFocus }

        Vty.EvKey (Vty.KChar 'a') [Vty.MMeta] -> do
            -- Construct the original list of ingredient options
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn
            is <- liftIO $ Ingredients.getIngredients conn
            let ingrListOrig = map (\(Types.IngredientClass i n) -> IngredientClassListItem i n) ics
                            ++ map (\(Types.Ingredient i n _ _ _ _) -> IngredientListItem i n) is

            -- Construct the UI elements
            let amountEditor = BE.editor RecipeInputIngredient.amountName (Just 1) ""
                unitEditor = BE.editor RecipeInputIngredient.unitName (Just 1) ""
                filterEditor = BE.editor RecipeInputIngredient.filterName (Just 1) ""
                ingList = BL.list RecipeInputIngredient.ingrListName (V.fromList ingrListOrig) 1
                f = BF.focusRing [ RecipeInputIngredient.amountName
                                 , RecipeInputIngredient.unitName
                                 , RecipeInputIngredient.filterName
                                 , RecipeInputIngredient.ingrListName
                                 ]
                name = case (mr,recipeFor) of
                          -- We're editing an existing recipe
                          (Just r,_) -> case Types.recipeName r of
                                          -- It's a standalone recipe
                                          Left n -> "\"" `T.append` n `T.append` "\""
                                          -- It's a recipe for an ingredient
                                          Right i -> "the recipe for \"" `T.append` Types.ingredientName i `T.append` "\""
                          -- It's a new recipe for an ingredient
                          (_,Just i) -> "the recipe for \"" `T.append` Types.ingredientName i `T.append` "\""
                          -- It's a new standalone recipe
                          (Nothing,Nothing) -> (\mn -> if isJust mn then "\"" `T.append` fromJust mn `T.append` "\"" else "\"\"") $ getEditorLine nameEd


            -- Give a function for getting back here
                getBack = (\mil -> case mil of
                                    Just i@(Types.IngredientListItem _ _ _ _) ->
                                        let newList = BL.listInsert (length il) i il
                                        in scr { recipeInputIngredientList = newList }
                                    Nothing -> scr)

            B.continue $ CyanideState conn (RecipeInputIngredientScreen name amountEditor unitEditor filterEditor ingrListOrig ingList f getBack)

        Vty.EvKey (Vty.KChar 'i') [Vty.MMeta] -> do
            editorEnv <- liftIO $ getEnv "EDITOR"
            let editor = case editorEnv of
                            "" -> "vim"
                            e -> e
                tmpDir = "/tmp"
                hashOfInstructions = showDigest $ sha512 (BSL.pack $ T.unpack instr)
                fileName = tmpDir ++ "/cyanide-" ++ take 8 hashOfInstructions ++ ".md"
            liftIO $ writeFile fileName (T.unpack instr)
            B.suspendAndResume $ do
                callCommand $ editor ++ " " ++ fileName
                newInstructions <- readFile fileName
                removeFile fileName
                return $ CyanideState conn $ scr { recipeInputInstructions = T.pack newInstructions }

        Vty.EvKey (Vty.KChar 'd') [Vty.MMeta] ->
            if BF.focusGetCurrent f /= Just ingredientsName || length il == 0
                then B.continue s
                else let Just (i,_) = BL.listSelectedElement il
                         newList = BL.listRemove i il
                     in B.continue $ CyanideState conn $ scr { recipeInputIngredientList = newList }

        Vty.EvKey (Vty.KEnter) [] -> do
            let name = getEditorLine nameEd
                garnish = case getEditorLine garnishEd of
                                Nothing -> ""
                                Just t -> t
                Just (_,mGlass) = BL.listSelectedElement gl
                ingredients = V.toList $ il^.(BL.listElementsL)
            case mr of
                Just oldRecipe -> do
                    newRecipe <- liftIO $ Recipes.updateRecipe conn (Types.recipeId oldRecipe) (name,garnish,instr,mGlass,recipeFor,ingredients)
                    newScr <- liftIO $ prev (Just (newRecipe,mGlass,ingredients))
                    B.continue $ CyanideState conn newScr
                Nothing -> do
                    newRecipe <- liftIO $ Recipes.newRecipe conn (name,garnish,instr,mGlass,recipeFor,ingredients)
                    newScr <- liftIO $ prev (Just (newRecipe,mGlass,ingredients))
                    B.continue $ CyanideState conn newScr

        ev -> if BF.focusGetCurrent (f) == Just recipeName then do
                    newEdit <- BE.handleEditorEvent ev nameEd
                    B.continue $ CyanideState conn $ scr { recipeInputName = newEdit }
              else if BF.focusGetCurrent (f) == Just garnishName then do
                    newEdit <- BE.handleEditorEvent ev garnishEd
                    B.continue $ CyanideState conn $ scr { recipeInputGarnish = newEdit }
              else if BF.focusGetCurrent (f) == Just glassName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev gl
                    B.continue $ CyanideState conn $ scr { recipeInputGlass = newList }
              else if BF.focusGetCurrent (f) == Just ingredientsName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev il
                    B.continue $ CyanideState conn $ scr { recipeInputIngredientList = newList }
              else B.continue s

  where ingredientListItemToName (Types.IngredientListItem _ _ _ (Left i)) =
            Types.ingredientName i
        ingredientListItemToName (Types.IngredientListItem _ _ _ (Right ic)) =
            Types.ingredientClassName ic

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (RecipeInputScreen nameEd garnishEd gl il instr recipeFor mr f _)) = [ui]
    where nameEdRndrd = BF.withFocusRing f (BE.renderEditor drawEdit) nameEd
          garnishEdRndrd = BF.withFocusRing f (BE.renderEditor drawEdit) garnishEd
          glst = BF.withFocusRing f (BL.renderList drawListGlass) gl
          ilst = BF.withFocusRing f (BL.renderList drawListIngredient) il

          prompt = case (mr,recipeFor) of
                    -- We're editing an existing recipe
                    (Just r,_) -> case Types.recipeName r of
                                    -- It's a standalone recipe
                                    Left n -> "How do you want to edit \"" `T.append` n `T.append` "\"?"
                                    -- It's a recipe for an ingredient
                                    Right i -> "How do you want to edit the recipe for \"" `T.append` Types.ingredientName i `T.append` "\"?"
                    -- It's a new recipe for an ingredient
                    (_,Just i) -> "What is the recipe for \"" `T.append` Types.ingredientName i `T.append` "\"?"
                    -- It's a new standalone recipe
                    (Nothing,Nothing) -> "What recipe do you want to create?"
        
          enterAction = case mr of
                    Just _ -> "Modify"
                    Nothing -> "Create"

          recipeInfo = B.hBox
                [ if isNothing recipeFor
                    then B.hLimit 20 $
                            B.vBox [ BB.borderWithLabel (B.txt "Name") nameEdRndrd
                                   , BB.borderWithLabel (B.txt "Garnish") garnishEdRndrd
                                   , BB.borderWithLabel (B.txt "Glass") glst
                                   ]
                    else B.emptyWidget
                , B.hLimit 30 $ BB.borderWithLabel (B.txt "Ingredients") ilst
                , BB.borderWithLabel (B.txt "Instructions") (B.padBottom (B.Max) $ B.padRight (B.Max) (B.txtWrap $ if instr == "" then " " else instr))
                ]

          instructionsLeft =
                B.vBox [ B.txt $ "Enter - " `T.append` enterAction
                       , B.txt "  Tab - Change focus"
                       , B.txt "  Esc - Cancel"
                       ]
          instructionsRight =
            B.padLeft (B.Pad 2) $
                B.vBox  $ 
                    if BF.focusGetCurrent f == Just ingredientsName
                        then [ B.txt "Alt-i - Edit instructions"
                             , B.txt "Alt-a - Add ingredient"
                             , B.txt "Alt-d - Delete ingredient"
                             ]
                        else [ B.txt "Alt-i - Edit instructions"
                             , B.txt "Alt-a - Add ingredient"
                             ]
          instructions = B.padLeft (B.Pad 16) $ B.hBox [ instructionsLeft, instructionsRight ]

          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt prompt
                            , BC.hCenter $ B.txt $ case recipeFor of
                                                       (Just r) -> "This is a recipe for: " `T.append` (Types.ingredientName r)
                                                       Nothing -> " "
                            , recipeInfo
                            , instructions
                            ]

drawEdit = B.txt . T.unlines

drawListGlass :: Bool -> (Maybe Types.Glass) -> B.Widget Name
drawListGlass False (Just (Types.Glass _ n)) = BC.hCenter $ B.txt n
drawListGlass True (Just (Types.Glass _ n)) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"
drawListGlass False Nothing = B.txt " "
drawListGlass True Nothing = BC.hCenter $ B.txt "*"

drawListIngredient :: Bool -> Types.IngredientListItem -> B.Widget Name
drawListIngredient _ ingr = B.txt $ formatIngr ingr
