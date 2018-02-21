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
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Postgres as Postgres

recipeName :: Name
recipeName = "RecipeCreationName"

glassName :: Name
glassName = "RecipeCreationGlassList"

ingredientsName :: Name
ingredientsName = "RecipeCreationIngredientList"

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(RecipeInputScreen nameEd gl il instr recipeFor mr f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn (prev Nothing)

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

            -- Give a function for getting back here
                getBack = (\mil -> case mil of
                                    Just i@(Types.IngredientListItem _ _ _ _) ->
                                        let newList = BL.listInsert (length il) i il
                                        in scr { recipeInputIngredientList = newList }
                                    Nothing -> scr)

            B.continue $ CyanideState conn (RecipeInputIngredientScreen "TODO" amountEditor unitEditor filterEditor ingrListOrig ingList f getBack)

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
                Just (_,g) = BL.listSelectedElement gl
                ingredients = V.toList $ il^.(BL.listElementsL)
            case mr of
                Just oldRecipe ->
                -- TODO
                    B.continue s
                Nothing -> do
                    newRecipe <- liftIO $ Recipes.newRecipe conn (name,instr,recipeFor,ingredients)
                    B.continue $ CyanideState conn $ prev (Just newRecipe)

        --Vty.EvKey (Vty.KEnter) [] -> do
        --    mIngredientName <- getAndCheckEditorName (isNothing mi)
        --    case (mi,mIngredientName) of
        --        (_,Nothing) -> B.continue s
        --        -- We're updating an existing ingredient
        --        (Just oldIng,Just n) -> do
        --            let Just (_,iclass) = BL.listSelectedElement cl
        --                Just (_,unit) = BL.listSelectedElement ul

        --            newIngredient <- liftIO $ Ingredients.updateIngredient conn (Types.ingredientId oldIng) (n,iclass,unit,si)
        --            let newList = BL.listModify (\_ -> newIngredient) pl
        --                newList' = BL.listMoveTo (length newList) newList
        --            B.continue $ CyanideState conn (IngredientSelectionScreen newList')
        --        -- We're creating a new ingredient
        --        (Nothing,Just n) -> do
        --            let Just (_,iclass) = BL.listSelectedElement cl
        --                Just (_,unit) = BL.listSelectedElement ul

        --            newIngredient <- liftIO $ Ingredients.newIngredient conn (n,iclass,unit,si)
        --            let newList = BL.listInsert (length pl) newIngredient pl
        --                newList' = BL.listMoveTo (length newList) newList
        --            B.continue $ CyanideState conn (IngredientSelectionScreen newList')

        ev -> if BF.focusGetCurrent (f) == Just recipeName then do
                    newEdit <- BE.handleEditorEvent ev nameEd
                    B.continue $ CyanideState conn $ scr { recipeInputName = newEdit }
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
  --where getAndCheckEditorName mustBeUnique = do
  --          let newIngredientNames = BE.getEditContents ed
  --          if length newIngredientNames /= 1
  --              then return Nothing
  --              else do
  --                  ingredients <- liftIO $ Ingredients.getIngredients conn
  --                  let newIngredientName = newIngredientNames !! 0
  --                  if not mustBeUnique
  --                      then return $ Just newIngredientName
  --                      else case filter (\i -> Types.ingredientName i == newIngredientName) ingredients of
  --                              [] -> return $ Just newIngredientName
  --                              _ -> return Nothing

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (RecipeInputScreen nameEd gl il instr recipeFor mr f _)) = [ui]
    where edt = BF.withFocusRing f (BE.renderEditor drawEdit) nameEd
          glst = BF.withFocusRing f (BL.renderList drawListGlass) gl
          ilst = BF.withFocusRing f (BL.renderList drawListIngredient) il

          prompt = case (mr,recipeFor) of
                    -- We're editing an existing recipe
                    (Just r,_) -> case Types.recipeName r of
                                    -- It's a standalong recipe
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
                            B.vBox [ BB.borderWithLabel (B.txt "Name") edt
                                   , BB.borderWithLabel (B.txt "Glass") glst
                                   ]
                    else B.emptyWidget
                , B.hLimit 30 $ BB.borderWithLabel (B.txt "Ingredients") ilst
                , BB.borderWithLabel (B.txt "Instructions") (B.padBottom (B.Max) $ B.padRight (B.Max) (B.txt $ if instr == "" then " " else instr))
                ]

          instructionsLeft =
                B.vBox [ B.txt $ "Enter - " `T.append` enterAction
                       , B.txt "Tab   - Change focus"
                       , B.txt "Esc   - Previous screen"
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

drawListGlass :: Bool -> Types.Glass -> B.Widget Name
drawListGlass False (Types.Glass _ n) = BC.hCenter $ B.txt n
drawListGlass True (Types.Glass _ n) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"

drawListIngredient :: Bool -> Types.IngredientListItem -> B.Widget Name
drawListIngredient _ ingr = B.txt $ formatIngr ingr
