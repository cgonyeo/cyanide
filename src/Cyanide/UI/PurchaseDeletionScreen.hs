{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.PurchaseDeletionScreen where

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
import qualified Cyanide.Data.Purchases as Purchases
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ (PurchaseDeletionScreen p i prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = prev False }

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ s { stateScreen = prev False }

        Vty.EvKey (Vty.KChar 'y') [] -> do
            liftIO $ Purchases.deletePurchase conn p
            B.continue $ s { stateScreen = prev True }

        _ -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (PurchaseDeletionScreen (Types.Purchase t l p a u) i _)) = [ui]
    where ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt $ "Are you sure you want to delete the following purchase?"
                            , BC.hCenter
                                $ B.padAll 1
                                $ BB.borderWithLabel (B.txt "Purchase")
                                $ B.padAll 1
                                $ B.vBox
                                     [ addRow 8 "Name" [B.txt $ Types.ingredientName i]
                                     , addRow 8 "Date" [B.txt $ T.pack $ show t]
                                     , addRow 8 "Location" [B.txt l]
                                     , addRow 8 "Price" [B.txt $ formatMoney p]
                                     , addRow 8 "Amount" [B.txt $ (T.pack (show a)) `T.append` u]
                                     ]
                            , renderInstructions [ ("y","Yes")
                                                 , ("n","No")
                                                 ]
                            ]
