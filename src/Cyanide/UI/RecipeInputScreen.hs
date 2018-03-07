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
import System.Posix.Env
import System.Process
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.SHA

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.Config as Config
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
handleEvent s@(CyanideState conn conf scr@(RecipeInputScreen nameEd garnishEd gl il instr recipeFor mr f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] -> do
            newScr <- liftIO $ prev Nothing
            B.continue $ s { stateScreen = newScr }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { recipeInputFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'a') [Vty.MMeta] -> do
            newScr <- liftIO $ RecipeInputIngredient.newRecipeInputIngredientScreen conn Nothing getBack
            B.continue $ s { stateScreen = newScr }
          where getBack mil = case mil of
                                    Just i ->
                                        let newList = BL.listInsert (length il) i il
                                        in scr { recipeInputIngredientList = newList }
                                    Nothing -> scr

        Vty.EvKey (Vty.KChar 'i') [Vty.MMeta] -> do
            mEditorEnv <- liftIO $ getEnv "EDITOR"
            let editor = case (Config.editor (Config.editorSection conf),mEditorEnv) of
                            ("",Just e) -> e
                            ("",Nothing) -> "vim"
                            (e,_) -> T.unpack e
                tmpDir = "/tmp"
                hashOfInstructions = showDigest $ sha512 (BSL.pack $ T.unpack instr)
                fileName = tmpDir ++ "/cyanide-" ++ take 8 hashOfInstructions ++ ".md"
            liftIO $ writeFile fileName (T.unpack instr)
            B.suspendAndResume $ do
                callCommand $ editor ++ " " ++ fileName
                newInstructions <- readFile fileName
                removeFile fileName
                return $ s { stateScreen = scr { recipeInputInstructions = T.pack newInstructions } }

        Vty.EvKey (Vty.KChar 'd') [Vty.MMeta] ->
            if BF.focusGetCurrent f /= Just ingredientsName || length il == 0
                then B.continue s
                else let Just (i,_) = BL.listSelectedElement il
                         newList = BL.listRemove i il
                     in B.continue $ s { stateScreen = scr { recipeInputIngredientList = newList } }

        Vty.EvKey (Vty.KChar 'e') [Vty.MMeta] ->
            if BF.focusGetCurrent f /= Just ingredientsName || length il == 0
                then B.continue s
                else do let Just (_,i) = BL.listSelectedElement il
                        newScr <- liftIO $ RecipeInputIngredient.newRecipeInputIngredientScreen conn (Just i) getBack
                        B.continue $ s { stateScreen = newScr }
          where getBack mil = case mil of
                                Nothing -> scr
                                Just i ->
                                    let newList = BL.listModify (\_ -> i) il
                                    in scr { recipeInputIngredientList = newList }

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
                    B.continue $ s { stateScreen = newScr }
                Nothing -> do
                    newRecipe <- liftIO $ Recipes.newRecipe conn (name,garnish,instr,mGlass,recipeFor,ingredients)
                    newScr <- liftIO $ prev (Just (newRecipe,mGlass,ingredients))
                    B.continue $ s { stateScreen = newScr }

        ev -> if BF.focusGetCurrent (f) == Just recipeName then do
                    newEdit <- BE.handleEditorEvent ev nameEd
                    B.continue $ s { stateScreen = scr { recipeInputName = newEdit } }
              else if BF.focusGetCurrent (f) == Just garnishName then do
                    newEdit <- BE.handleEditorEvent ev garnishEd
                    B.continue $ s { stateScreen = scr { recipeInputGarnish = newEdit } }
              else if BF.focusGetCurrent (f) == Just glassName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev gl
                    B.continue $ s { stateScreen = scr { recipeInputGlass = newList } }
              else if BF.focusGetCurrent (f) == Just ingredientsName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev il
                    B.continue $ s { stateScreen = scr { recipeInputIngredientList = newList } }
              else B.continue s

  where ingredientListItemToName (Types.IngredientListItem _ _ _ (Left i)) =
            Types.ingredientName i
        ingredientListItemToName (Types.IngredientListItem _ _ _ (Right ic)) =
            Types.ingredientClassName ic

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (RecipeInputScreen nameEd garnishEd gl il instr recipeFor mr f _)) = [ui]
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
                    then B.hLimit 21 $
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
                             , B.txt "Alt-e - Edit ingredient"
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
