{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientClassDeletionScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
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
handleEvent s@(CyanideState conn _ (IngredientClassDeletionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = (IngredientClassSelectionScreen l) }

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ s { stateScreen = (IngredientClassSelectionScreen l) }

        Vty.EvKey (Vty.KChar 'y') [] -> do
            let Just (i,glass) = BL.listSelectedElement l
                newList = BL.listRemove i l
            liftIO $ IngredientClasses.deleteIngredientClass conn glass
            B.continue $ s { stateScreen = (IngredientClassSelectionScreen newList) }

        _ -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientClassDeletionScreen l)) = [ui]
    where Just (_,(Types.IngredientClass _ n)) = BL.listSelectedElement l
          ui = B.vBox [ BC.hCenter $ B.txt $ "Are you sure you want to delete the following ingredient class?"
                      , BC.hCenter $ B.padAll 1 $ B.txt n
                      , renderInstructions [ ("y","Yes")
                                           , ("n","No")
                                           ]
                      ]
