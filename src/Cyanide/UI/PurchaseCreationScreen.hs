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
import Data.Time.LocalTime

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

amountEditorName :: Name
amountEditorName = "PurchaseCreationAmount"

unitEditorName :: Name
unitEditorName = "PurchaseCreationUnit"

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(PurchaseCreationScreen ing le ce ae ue fe prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey Vty.KEsc [] ->
            B.continue $ s { stateScreen = prev Nothing }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext fe
            in B.continue $ s { stateScreen = scr { purchaseCreationEditFocusRing = newFocus } }

        Vty.EvKey Vty.KEnter [] ->
            case map getEditorLine [le,ce,ae,ue] of
                [Just l,Just c,Just a, Just u] ->
                    case (readMaybe (T.unpack $ c),readMaybe (T.unpack a)) of
                        (Nothing,_) -> B.continue $ s { stateScreen = ErrorScreen "Couldn't parse the cost. Please enter the cost in cents, so $12.95 would become 1295." scr }
                        (_,Nothing) -> B.continue $ s { stateScreen = ErrorScreen "Couldn't parse the amount. Please enter an integer." scr }
                        (Just cn,Just an) ->
                            if cn < 0 || an < 0 || cn > 999999 || an > 999999
                                then B.continue $ s { stateScreen = ErrorScreen "The cost and amount must be between 0 and 999999." scr }
                                else do
                                    -- mark the ingredient as available
                                    liftIO $ Ingredients.updateIngredientAvailability conn (ing,True)
                                    let newIngredient = ing { Types.available = True }

                                    -- create the purchase
                                    now <- liftIO getZonedTime
                                    let today = localDay $ zonedTimeToLocalTime now
                                    liftIO $ Purchases.newPurchase conn (ing,today,l,cn,an,u)
                                    B.continue $ s { stateScreen = prev (Just (newIngredient,Types.Purchase today l cn an u)) }
                _ -> B.continue s

        ev -> if BF.focusGetCurrent (fe) == Just locationEditorName then do
                    newEdit <- BE.handleEditorEvent e le
                    B.continue $ s { stateScreen = scr { purchaseCreationEditLocation = newEdit } }
              else if BF.focusGetCurrent (fe) == Just costEditorName then do
                    newEdit <- BE.handleEditorEvent e ce
                    B.continue $ s { stateScreen = scr { purchaseCreationEditCost = newEdit } }
              else if BF.focusGetCurrent (fe) == Just amountEditorName then do
                    newEdit <- BE.handleEditorEvent e ae
                    B.continue $ s { stateScreen = scr { purchaseCreationEditAmount = newEdit } }
              else if BF.focusGetCurrent (fe) == Just unitEditorName then do
                    newEdit <- BE.handleEditorEvent e ue
                    B.continue $ s { stateScreen = scr { purchaseCreationEditUnit = newEdit } }
              else B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (PurchaseCreationScreen ing le ce ae ue fe _)) =
    [ B.vBox
            [ BC.hCenter $ B.txt $ "Where did you buy " `T.append` Types.ingredientName ing `T.append` " and how much was it?"
            , B.txt " "
            , BC.hCenter $ B.txt "Location:"
            , BC.hCenter $ BB.border $ B.hLimit 17 $ BF.withFocusRing fe (BE.renderEditor drawEdit) le
            , B.txt " "
            , BC.hCenter $ B.txt "Cost (in cents):"
            , BC.hCenter $ BB.border $ B.hLimit 9 $ BF.withFocusRing fe (BE.renderEditor drawEdit) ce
            , B.txt " "
            , BC.hCenter $ B.txt "Amount:"
            , BC.hCenter $ BB.border $ B.hLimit 9 $ BF.withFocusRing fe (BE.renderEditor drawEdit) ae
            , B.txt " "
            , BC.hCenter $ B.txt "Amount unit:"
            , BC.hCenter $ BB.border $ B.hLimit 9 $ BF.withFocusRing fe (BE.renderEditor drawEdit) ue
            , B.txt " "
            , renderInstructions [ ("Tab","Change focus")
                                 , ("Enter","Record purchase")
                                 , ("Esc","Cancel")
                                 ]
            ]
    ]
  where drawEdit = B.txt . T.unlines
