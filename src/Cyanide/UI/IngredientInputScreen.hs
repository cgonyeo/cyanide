{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientInputScreen where

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

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Postgres as Postgres

editorName :: Name
editorName = "IngredientCreationName"

classesName :: Name
classesName = "IngredientCreationClassList"

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientInputScreen ed cl f si mi prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = prev Nothing }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { ingredientInputFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'c') [Vty.MMeta] ->
            B.continue $ s { stateScreen = scr { ingredientInputNotForRecipes = not si } }

        Vty.EvKey (Vty.KEnter) [] -> do
            mIngredientName <- getAndCheckEditorName (isNothing mi)
            case (mi,mIngredientName) of
                (_,Nothing) -> B.continue s
                -- We're updating an existing ingredient
                (Just oldIng,Just n) -> do
                    let Just (_,iclass) = BL.listSelectedElement cl

                    newIngredient <- liftIO $ Ingredients.updateIngredient conn (Types.ingredientId oldIng) (n,iclass,si)
                    B.continue $ s { stateScreen = prev (Just (newIngredient,iclass)) }
                -- We're creating a new ingredient
                (Nothing,Just n) -> do
                    let Just (_,iclass) = BL.listSelectedElement cl

                    newIngredient <- liftIO $ Ingredients.newIngredient conn (n,iclass,si)
                    B.continue $ s { stateScreen = prev (Just (newIngredient,iclass)) }

        ev -> if BF.focusGetCurrent (f) == Just editorName then do
                    newEdit <- BE.handleEditorEvent ev ed
                    B.continue $ s { stateScreen = scr { ingredientInputName = newEdit } }
              else if BF.focusGetCurrent (f) == Just classesName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev cl
                    B.continue $ s { stateScreen = scr { ingredientInputClass = newList } }
              else B.continue s

  where getAndCheckEditorName mustBeUnique = do
            let newIngredientNames = BE.getEditContents ed
            if length newIngredientNames /= 1
                then return Nothing
                else do
                    ingredients <- liftIO $ Ingredients.getIngredients conn
                    let newIngredientName = newIngredientNames !! 0
                    if not mustBeUnique
                        then return $ Just newIngredientName
                        else case filter (\i -> Types.ingredientName i == newIngredientName) ingredients of
                                [] -> return $ Just newIngredientName
                                _ -> return Nothing

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientInputScreen e cl f s mi _)) = [ui]
    where edt = BF.withFocusRing f (BE.renderEditor drawEdit) e
          clst = BF.withFocusRing f (BL.renderList drawListClass) cl

          recipeState = if s then B.txt "Not for use in cocktails"
                             else B.txt "Available to cocktails"

          prompt = case mi of
                    Just i -> "How do you want to edit \"" `T.append` Types.ingredientName i `T.append` "\"?"
                    Nothing -> "What ingredient do you want to create?"
        
          enterAction = case mi of
                    Just _ -> "Modify"
                    Nothing -> "Create"

          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt prompt
                            , BC.hCenter $ B.hLimit 24 $ B.padAll 1
                                $ B.vBox [ BC.hCenter $ B.txt "Name"
                                         , BB.border $ edt
                                         ]
                            , BC.hCenter $ B.padBottom (B.Pad 1) $ recipeState
                            , B.vBox [ BC.hCenter $ B.txt "Class"
                                     , BB.border $ clst
                                     ]
                            , renderInstructions [ ("Enter",enterAction)
                                                 , ("Alt-c","Toggle cocktail availability")
                                                 , ("Tab","Change focus")
                                                 , ("Esc","Cancel")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines

drawListClass:: Bool -> Maybe Types.IngredientClass -> B.Widget Name
drawListClass False Nothing = BC.hCenter $ B.txt " "
drawListClass True Nothing = BC.hCenter $ B.txt "*"
drawListClass False (Just (Types.IngredientClass _ n)) = BC.hCenter $ B.txt n
drawListClass True (Just (Types.IngredientClass _ n)) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"
