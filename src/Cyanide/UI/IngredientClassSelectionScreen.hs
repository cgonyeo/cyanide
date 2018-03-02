{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientClassSelectionScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import Data.Monoid
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientClassSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey (Vty.KChar 'd') [] ->
            case BL.listSelectedElement l of
                Nothing -> B.continue s
                Just (_,ic) -> do
                    isInUse <- liftIO $ IngredientClasses.isIngredientClassInUse conn ic
                    if isInUse
                        then B.continue $ s { stateScreen = (ErrorScreen "There are ingredients and/or recipes using that ingredient class." scr) }
                        else B.continue $ s { stateScreen = (IngredientClassDeletionScreen l) }

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ s { stateScreen = (IngredientClassInputScreen (BE.editorText "IngredientClassCreationScreen" (Just 1) "") Nothing l) }

        Vty.EvKey (Vty.KChar 'e') [] ->
            let (Just (_, ic@(Types.IngredientClass _ n))) = BL.listSelectedElement l
            in B.continue $ s { stateScreen = (IngredientClassInputScreen (BE.editorText "IngredientClassModificationScreen" (Just 1) n) (Just ic) l) }

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ s { stateScreen = (IngredientClassSelectionScreen newList) }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientClassSelectionScreen l)) = [ui]
    where box = BB.borderWithLabel (B.txt "Ingredient Classes") $
              BL.renderList listDrawElement True l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter box
                            , renderInstructions [ ("n","New ingredient class")
                                                 , ("e","Edit ingredient class")
                                                 , ("d","Delete ingredient class")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.IngredientClass -> B.Widget Name
listDrawElement _ (Types.IngredientClass _ n) = BC.hCenter $ B.txt n
