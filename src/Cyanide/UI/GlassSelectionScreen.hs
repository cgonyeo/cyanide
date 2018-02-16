{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.GlassSelectionScreen where

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
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn (GlassSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn MainSelectionScreen

        Vty.EvKey (Vty.KChar 'd') [] ->
            B.continue $ CyanideState conn (GlassDeletionScreen l)

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ CyanideState conn (GlassCreationScreen (BE.editorText "GlassCreationScreen" (Just 1) "") l)

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ CyanideState conn (GlassSelectionScreen newList)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (GlassSelectionScreen l)) = [ui]
    where box = BB.borderWithLabel (B.txt "Glasses") $
              BL.renderList listDrawElement True l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter box
                            , renderInstructions [ ("n","New glass")
                                                 , ("d","Delete glass")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Glass -> B.Widget Name
listDrawElement sel (Types.Glass _ n) = BC.hCenter $ B.str $ T.unpack n
