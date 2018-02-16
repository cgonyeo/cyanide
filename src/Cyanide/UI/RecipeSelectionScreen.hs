{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeSelectionScreen where

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
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(RecipeSelectionScreen l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn MainSelectionScreen

        Vty.EvKey (Vty.KChar 'n') [] ->
            let newList = BL.listInsert 0 (Types.Recipe 0 "test" "testy") l
            in B.continue $ CyanideState conn (RecipeSelectionScreen newList)

        Vty.EvKey (Vty.KEnter) [] ->
            case (BL.listSelectedElement l) of
                Nothing -> B.continue s
                Just (_,r) -> do
                    glass <- liftIO $ Recipes.getGlassForRecipe conn r
                    ingrs <- liftIO $ Recipes.getIngredientsForRecipe conn r
                    B.continue $ CyanideState conn $ RecipeDetailScreen r glass ingrs scr

        ev -> do
            newList <- BL.handleListEventVi BL.handleListEvent ev l
            B.continue $ CyanideState conn (RecipeSelectionScreen newList)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (RecipeSelectionScreen l)) = [ui]
    where label = B.str "Recipe " B.<+> cur B.<+> B.str " of " B.<+> total
          cur = case (BL.listSelectedElement l) of
                Nothing -> B.str "-"
                Just (i,_) -> B.str (show (i + 1))
          total = B.str $ show $ V.length $ l^.(BL.listElementsL)
          box = BB.borderWithLabel label $
              BL.renderList listDrawElement True l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter box
                            , renderInstructions [ ("f","Filter list")
                                                 , ("n","New recipe")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Recipe -> B.Widget Name
listDrawElement sel (Types.Recipe _ n _) = BC.hCenter $ B.str $ T.unpack n
