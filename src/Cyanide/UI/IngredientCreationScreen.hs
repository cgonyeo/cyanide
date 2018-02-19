{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientCreationScreen where

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

unitsName :: Name
unitsName = "IngredientCreationUnitList"

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(IngredientCreationScreen ed cl ul f si pl)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn (IngredientSelectionScreen pl)

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ CyanideState conn $ scr { ingredientCreationFocusRing = newFocus }

        Vty.EvKey (Vty.KChar 'c') [Vty.MMeta] ->
            B.continue $ CyanideState conn $ scr { ingredientCreationNotForRecipes = not si }

        Vty.EvKey (Vty.KEnter) [] -> do
            mIngredientName <- getAndCheckEditorName
            case mIngredientName of
                Nothing -> B.continue s
                Just i -> do
                    let Just (_,iclass) = BL.listSelectedElement cl
                        Just (_,unit) = BL.listSelectedElement ul

                    newIngredient <- liftIO $ Ingredients.newIngredient conn (i,iclass,unit,si)
                    let newList = BL.listInsert (length pl) newIngredient pl
                        newList' = BL.listMoveTo (length newList) newList
                    B.continue $ CyanideState conn (IngredientSelectionScreen newList')

        ev -> if BF.focusGetCurrent (f) == Just editorName then do
                    newEdit <- BE.handleEditorEvent ev ed
                    B.continue $ CyanideState conn $ scr { ingredientCreationName = newEdit }
              else if BF.focusGetCurrent (f) == Just classesName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev cl
                    B.continue $ CyanideState conn $ scr { ingredientCreationClass = newList }
              else if BF.focusGetCurrent (f) == Just unitsName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev ul
                    B.continue $ CyanideState conn $ scr { ingredientCreationUnit = newList }
              else B.continue s

  where getAndCheckEditorName = do
            let newIngredientNames = BE.getEditContents ed
            if length newIngredientNames /= 1
                then return Nothing
                else do
                    ingredients <- liftIO $ Ingredients.getIngredients conn
                    let newIngredientName = newIngredientNames !! 0
                    case filter (\i -> Types.ingredientName i == newIngredientName) ingredients of
                        [] -> return $ Just newIngredientName
                        _ -> return Nothing

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (IngredientCreationScreen e cl ul f s pl)) = [ui]
    where edt = BF.withFocusRing f (BE.renderEditor drawEdit) e
          clst = BF.withFocusRing f (BL.renderList drawListClass) cl
          ulst = BF.withFocusRing f (BL.renderList drawListUnit) ul

          recipeState = if s then B.txt "Not for use in cocktails"
                             else B.txt "Available to cocktails"

          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt $ "What ingredient do you want to create?"
                            , BC.hCenter $ B.hLimit 24 $ B.padAll 1
                                $ B.vBox [ BC.hCenter $ B.txt "Name"
                                         , BB.border $ edt
                                         ]
                            , BC.hCenter $ B.padBottom (B.Pad 1) $ recipeState
                            , B.hBox [ B.vBox [ BC.hCenter $ B.txt "Class"
                                                     , BB.border $ clst
                                                     ]
                                      , B.vBox [ BC.hCenter $ B.txt "Unit"
                                                     , BB.border $ ulst
                                                     ]
                                      ]
                            , renderInstructions [ ("Enter","Create")
                                                 , ("Alt-c","Toggle cocktail availability")
                                                 , ("Tab","Change focus")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines

drawListClass:: Bool -> Types.IngredientClass -> B.Widget Name
drawListClass False (Types.IngredientClass _ n) = BC.hCenter $ B.txt n
drawListClass True (Types.IngredientClass _ n) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"

drawListUnit :: Bool -> T.Text -> B.Widget Name
drawListUnit False "" = BC.hCenter $ B.txt " "
drawListUnit True "" = BC.hCenter $ B.txt "*"
drawListUnit False t = BC.hCenter $ B.txt t
drawListUnit True t = BC.hCenter $ B.txt $ "* " `T.append` t `T.append` " *"
