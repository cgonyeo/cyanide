{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.App where

import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Vector as V
import Control.Monad

import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Postgres as Postgres

import Cyanide.UI.State

import qualified Cyanide.UI.RecipeSelectionScreen as RecipeSelectionScreen
import qualified Cyanide.UI.RecipeDetailScreen as RecipeDetailScreen
import qualified Cyanide.UI.MainSelectionScreen as MainSelectionScreen
import qualified Cyanide.UI.GlassSelectionScreen as GlassSelectionScreen
import qualified Cyanide.UI.GlassDeletionScreen as GlassDeletionScreen
import qualified Cyanide.UI.GlassInputScreen as GlassInputScreen
import qualified Cyanide.UI.IngredientSelectionScreen as IngredientSelectionScreen
import qualified Cyanide.UI.IngredientCreationScreen as IngredientCreationScreen
import qualified Cyanide.UI.IngredientDetailScreen as IngredientDetailScreen
import qualified Cyanide.UI.PurchaseDeletionScreen as PurchaseDeletionScreen
import qualified Cyanide.UI.PurchaseCreationScreen as PurchaseCreationScreen
import qualified Cyanide.UI.IngredientClassSelectionScreen as IngredientClassSelectionScreen
import qualified Cyanide.UI.IngredientClassDeletionScreen as IngredientClassDeletionScreen
import qualified Cyanide.UI.IngredientClassCreationScreen as IngredientClassCreationScreen

run :: Postgres.DBConn -> IO ()
run conn = do
    let initialState = CyanideState conn MainSelectionScreen
    void $ B.defaultMain app initialState

app :: B.App CyanideState () Name
app = B.App { B.appDraw         = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent  = handleEvent
            , B.appStartEvent   = return
            , B.appAttrMap      = const attrMap
            }

attrMap :: B.AttrMap
attrMap = B.attrMap Vty.defAttr
    ( [ (BL.listAttr,                Vty.white `B.on` Vty.black)
      , (BL.listSelectedAttr,        Vty.white `B.on` Vty.black)
      , (BL.listSelectedFocusedAttr, Vty.black `B.on` Vty.white)
      ]
   ++ MainSelectionScreen.attrMap
   ++ RecipeDetailScreen.attrMap
   ++ RecipeSelectionScreen.attrMap
   ++ GlassSelectionScreen.attrMap
   ++ GlassDeletionScreen.attrMap
   ++ GlassInputScreen.attrMap
   ++ IngredientSelectionScreen.attrMap
   ++ IngredientCreationScreen.attrMap
   ++ IngredientDetailScreen.attrMap
   ++ PurchaseDeletionScreen.attrMap
   ++ PurchaseCreationScreen.attrMap
   ++ IngredientClassSelectionScreen.attrMap
   ++ IngredientClassDeletionScreen.attrMap
   ++ IngredientClassCreationScreen.attrMap
    )

-- Handling events

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState _ MainSelectionScreen) e =
    MainSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (GlassSelectionScreen _)) e =
    GlassSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (GlassDeletionScreen _)) e =
    GlassDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (GlassInputScreen _ _ _)) e =
    GlassInputScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeSelectionScreen _)) e =
    RecipeSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeDetailScreen _ _ _ _)) e =
    RecipeDetailScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientSelectionScreen _)) e =
    IngredientSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientCreationScreen _ _ _ _ _ _)) e =
    IngredientCreationScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientDetailScreen _ _ _ _ _ _)) e =
    IngredientDetailScreen.handleEvent s e
handleEvent s@(CyanideState _ (PurchaseDeletionScreen _ _ _ _ _ _)) e =
    PurchaseDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (PurchaseCreationScreen _ _ _ _ _ _ _ _ _)) e =
    PurchaseCreationScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassSelectionScreen _)) e =
    IngredientClassSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassDeletionScreen _)) e =
    IngredientClassDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassCreationScreen _ _)) e =
    IngredientClassCreationScreen.handleEvent s e

-- Drawing

drawUI :: CyanideState -> [B.Widget Name]
drawUI s@(CyanideState _ MainSelectionScreen) =
    MainSelectionScreen.drawUI s
drawUI s@(CyanideState _ (GlassSelectionScreen _)) =
    GlassSelectionScreen.drawUI s
drawUI s@(CyanideState _ (GlassDeletionScreen _)) =
    GlassDeletionScreen.drawUI s
drawUI s@(CyanideState _ (GlassInputScreen _ _ _)) =
    GlassInputScreen.drawUI s
drawUI s@(CyanideState _ (RecipeSelectionScreen _)) =
    RecipeSelectionScreen.drawUI s
drawUI s@(CyanideState _ (RecipeDetailScreen _ _ _ _)) =
    RecipeDetailScreen.drawUI s
drawUI s@(CyanideState _ (IngredientSelectionScreen _)) =
    IngredientSelectionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientCreationScreen _ _ _ _ _ _)) =
    IngredientCreationScreen.drawUI s
drawUI s@(CyanideState _ (IngredientDetailScreen _ _ _ _ _ _)) =
    IngredientDetailScreen.drawUI s
drawUI s@(CyanideState _ (PurchaseDeletionScreen _ _ _ _ _ _)) =
    PurchaseDeletionScreen.drawUI s
drawUI s@(CyanideState _ (PurchaseCreationScreen _ _ _ _ _ _ _ _ _)) =
    PurchaseCreationScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassSelectionScreen _)) =
    IngredientClassSelectionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassDeletionScreen _)) =
    IngredientClassDeletionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassCreationScreen _ _)) =
    IngredientClassCreationScreen.drawUI s
