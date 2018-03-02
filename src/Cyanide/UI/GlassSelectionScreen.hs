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
handleEvent s@(CyanideState conn _ scr@(GlassSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey (Vty.KChar 'd') [] -> do
            case BL.listSelectedElement l of
                Nothing -> B.continue s
                Just (_,g) -> do
                    isInUse <- liftIO $ Glasses.isGlassInUse conn g
                    if isInUse
                        then B.continue $ s { stateScreen = (ErrorScreen "That glass is being used in recipes." scr) }
                        else B.continue $ s { stateScreen = (GlassDeletionScreen l) }

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ s { stateScreen = (GlassInputScreen (BE.editorText "GlassCreationScreen" (Just 1) "") Nothing l) }

        Vty.EvKey (Vty.KChar 'e') [] ->
            let (Just (_, g@(Types.Glass _ n))) = BL.listSelectedElement l
            in B.continue $ s { stateScreen = (GlassInputScreen (BE.editorText "GlassModificationScreen" (Just 1) n) (Just g) l) }

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ s { stateScreen = (GlassSelectionScreen newList) }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (GlassSelectionScreen l)) = [ui]
    where box = BB.borderWithLabel (B.txt "Glasses") $
              BL.renderList listDrawElement True l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter box
                            , renderInstructions [ ("n","New glass")
                                                 , ("e","Edit glass")
                                                 , ("d","Delete glass")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Glass -> B.Widget Name
listDrawElement sel (Types.Glass _ n) = BC.hCenter $ B.str $ T.unpack n
