{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientClassCreationScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
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
handleEvent s@(CyanideState conn (IngredientClassCreationScreen ed l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn (IngredientClassSelectionScreen l)

        Vty.EvKey (Vty.KEnter) [] -> do
            let newIngredientClassNames = BE.getEditContents ed
            if length newIngredientClassNames /= 1
                then B.continue s
                else do
                    glasses <- liftIO $ IngredientClasses.getIngredientClasses conn
                    let newIngredientClassName = newIngredientClassNames !! 0
                    case filter (\(Types.IngredientClass _ n) -> n == newIngredientClassName) glasses of
                        [] -> do
                            newIngredientClass <- liftIO $ IngredientClasses.newIngredientClass conn newIngredientClassName
                            let newList = BL.listInsert (length l) newIngredientClass l
                            B.continue $ CyanideState conn (IngredientClassSelectionScreen newList)

        ev -> do
            newEdit <- BE.handleEditorEvent e ed
            B.continue $ CyanideState conn (IngredientClassCreationScreen newEdit l)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (IngredientClassCreationScreen e l)) = [ui]
    where Just (_,(Types.IngredientClass _ n)) = BL.listSelectedElement l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt $ "What ingredient class do you want to create?"
                            , BC.hCenter $ B.hLimit 24 $ B.padAll 1 $ BB.border $ BE.renderEditor drawEdit True e
                            , renderInstructions [ ("Enter","Create")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines
