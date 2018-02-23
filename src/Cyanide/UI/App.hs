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
import qualified Cyanide.UI.RecipeSelectionFilterScreen as RecipeSelectionFilterScreen
import qualified Cyanide.UI.RecipeDetailScreen as RecipeDetailScreen
import qualified Cyanide.UI.RecipeInputScreen as RecipeInputScreen
import qualified Cyanide.UI.RecipeInputIngredientScreen as RecipeInputIngredientScreen
import qualified Cyanide.UI.RecipeDeletionScreen as RecipeDeletionScreen
import qualified Cyanide.UI.MainSelectionScreen as MainSelectionScreen
import qualified Cyanide.UI.GlassSelectionScreen as GlassSelectionScreen
import qualified Cyanide.UI.GlassDeletionScreen as GlassDeletionScreen
import qualified Cyanide.UI.GlassInputScreen as GlassInputScreen
import qualified Cyanide.UI.IngredientSelectionScreen as IngredientSelectionScreen
import qualified Cyanide.UI.IngredientInputScreen as IngredientInputScreen
import qualified Cyanide.UI.IngredientDetailScreen as IngredientDetailScreen
import qualified Cyanide.UI.IngredientDeletionScreen as IngredientDeletionScreen
import qualified Cyanide.UI.PurchaseDeletionScreen as PurchaseDeletionScreen
import qualified Cyanide.UI.PurchaseCreationScreen as PurchaseCreationScreen
import qualified Cyanide.UI.IngredientClassSelectionScreen as IngredientClassSelectionScreen
import qualified Cyanide.UI.IngredientClassDeletionScreen as IngredientClassDeletionScreen
import qualified Cyanide.UI.IngredientClassInputScreen as IngredientClassInputScreen

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
   ++ RecipeInputScreen.attrMap
   ++ RecipeInputIngredientScreen.attrMap
   ++ RecipeDeletionScreen.attrMap
   ++ RecipeSelectionScreen.attrMap
   ++ RecipeSelectionFilterScreen.attrMap
   ++ GlassSelectionScreen.attrMap
   ++ GlassDeletionScreen.attrMap
   ++ GlassInputScreen.attrMap
   ++ IngredientSelectionScreen.attrMap
   ++ IngredientInputScreen.attrMap
   ++ IngredientDetailScreen.attrMap
   ++ IngredientDeletionScreen.attrMap
   ++ PurchaseDeletionScreen.attrMap
   ++ PurchaseCreationScreen.attrMap
   ++ IngredientClassSelectionScreen.attrMap
   ++ IngredientClassDeletionScreen.attrMap
   ++ IngredientClassInputScreen.attrMap
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
handleEvent s@(CyanideState _ (RecipeSelectionScreen _ _ _ _ _ _ _ _)) e =
    RecipeSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeSelectionFilterScreen _ _ _)) e =
    RecipeSelectionFilterScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeDetailScreen _ _ _ _)) e =
    RecipeDetailScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeInputScreen _ _ _ _ _ _ _ _ _)) e =
    RecipeInputScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeInputIngredientScreen _ _ _ _ _ _ _ _)) e =
    RecipeInputIngredientScreen.handleEvent s e
handleEvent s@(CyanideState _ (RecipeDeletionScreen _ _)) e =
    RecipeDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientSelectionScreen _)) e =
    IngredientSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientInputScreen _ _ _ _ _ _ _)) e =
    IngredientInputScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientDetailScreen _ _ _ _ _ _ _)) e =
    IngredientDetailScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientDeletionScreen _ _ _ _)) e =
    IngredientDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (PurchaseDeletionScreen _ _ _)) e =
    PurchaseDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (PurchaseCreationScreen _ _ _ _ _)) e =
    PurchaseCreationScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassSelectionScreen _)) e =
    IngredientClassSelectionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassDeletionScreen _)) e =
    IngredientClassDeletionScreen.handleEvent s e
handleEvent s@(CyanideState _ (IngredientClassInputScreen _ _ _)) e =
    IngredientClassInputScreen.handleEvent s e

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
drawUI s@(CyanideState _ (RecipeSelectionScreen _ _ _ _ _ _ _ _)) =
    RecipeSelectionScreen.drawUI s
drawUI s@(CyanideState _ (RecipeSelectionFilterScreen _ _ _)) =
    RecipeSelectionFilterScreen.drawUI s
drawUI s@(CyanideState _ (RecipeDetailScreen _ _ _ _)) =
    RecipeDetailScreen.drawUI s
drawUI s@(CyanideState _ (RecipeInputScreen _ _ _ _ _ _ _ _ _)) =
    RecipeInputScreen.drawUI s
drawUI s@(CyanideState _ (RecipeInputIngredientScreen _ _ _ _ _ _ _ _)) =
    RecipeInputIngredientScreen.drawUI s
drawUI s@(CyanideState _ (RecipeDeletionScreen _ _)) =
    RecipeDeletionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientSelectionScreen _)) =
    IngredientSelectionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientInputScreen _ _ _ _ _ _ _)) =
    IngredientInputScreen.drawUI s
drawUI s@(CyanideState _ (IngredientDetailScreen _ _ _ _ _ _ _)) =
    IngredientDetailScreen.drawUI s
drawUI s@(CyanideState _ (IngredientDeletionScreen _ _ _ _)) =
    IngredientDeletionScreen.drawUI s
drawUI s@(CyanideState _ (PurchaseDeletionScreen _ _ _)) =
    PurchaseDeletionScreen.drawUI s
drawUI s@(CyanideState _ (PurchaseCreationScreen _ _ _ _ _)) =
    PurchaseCreationScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassSelectionScreen _)) =
    IngredientClassSelectionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassDeletionScreen _)) =
    IngredientClassDeletionScreen.drawUI s
drawUI s@(CyanideState _ (IngredientClassInputScreen _ _ _)) =
    IngredientClassInputScreen.drawUI s
