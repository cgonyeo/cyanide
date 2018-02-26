{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.PurchaseCreationScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class
import Text.Read
import Data.Time.Clock
import Data.Time.Calendar

import Cyanide.UI.State
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Purchases as Purchases
import qualified Cyanide.Data.Postgres as Postgres
import Cyanide.UI.Util

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

locationEditorName :: Name
locationEditorName = "PurchaseCreationLocation"

costEditorName :: Name
costEditorName = "PurchaseCreationCost"

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(PurchaseCreationScreen ing le ce fe prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey Vty.KEsc [] ->
            B.continue $ s { stateScreen = prev Nothing }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext fe
            in B.continue $ s { stateScreen = scr { purchaseCreationEditFocusRing = newFocus } }

        Vty.EvKey Vty.KEnter [] ->
            let locations = BE.getEditContents le
                costs = BE.getEditContents ce
            in if length locations /= 1 || length costs /= 1
                    then B.continue s
                    else case readMaybe (T.unpack $ costs !! 0) of
                            Nothing -> B.continue s
                            Just p ->
                                if p < 0 || p > 999999
                                    then B.continue s
                                    else do
                                        -- update the amount count for the ingredient
                                        let newAmount = Types.amount ing + 1
                                        liftIO $ Ingredients.updateIngredientAmount conn (ing,newAmount)
                                        let newIngredient = ing { Types.amount = newAmount }

                                        -- create the purchase
                                        now <- liftIO getCurrentTime
                                        let today = utctDay now
                                            location = locations !! 0
                                        liftIO $ Purchases.newPurchase conn (ing,today,location,p)
                                        B.continue $ s { stateScreen = prev (Just (newIngredient,Types.Purchase today location p)) }

        ev -> if BF.focusGetCurrent (fe) == Just locationEditorName then do
                    newEdit <- BE.handleEditorEvent e le
                    B.continue $ s { stateScreen = scr { purchaseCreationEditLocation = newEdit } }
              else if BF.focusGetCurrent (fe) == Just costEditorName then do
                    newEdit <- BE.handleEditorEvent e ce
                    B.continue $ s { stateScreen = scr { purchaseCreationEditCost = newEdit } }
              else B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (PurchaseCreationScreen ing le ce fe _)) =
    [ BC.center
        $ B.hLimit 80
        $ B.vLimit 25
        $ B.vBox
            [ BC.hCenter $ B.txt $ "Where did you buy " `T.append` Types.ingredientName ing `T.append` " and how much was it?"
            , B.txt " "
            , BC.hCenter $ B.txt "Location:"
            , BC.hCenter $ BB.border $ B.hLimit 17 $ BF.withFocusRing fe (BE.renderEditor drawEdit) le
            , B.txt " "
            , BC.hCenter $ B.txt "Cost (in cents):"
            , BC.hCenter $ BB.border $ B.hLimit 9 $ BF.withFocusRing fe (BE.renderEditor drawEdit) ce
            , B.txt " "
            , renderInstructions [ ("Tab","Change focus")
                                 , ("Enter","Record purchase")
                                 , ("Esc","Cancel")
                                 ]
            ]
    ]
  where drawEdit = B.txt . T.unlines
