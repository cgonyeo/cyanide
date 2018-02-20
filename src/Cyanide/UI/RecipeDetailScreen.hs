{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeDetailScreen where

import qualified Brick as B
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
handleEvent s@(CyanideState conn scr@(RecipeDetailScreen _ _ _ prev)) (B.VtyEvent e) =
    case e of
        -- edit
        --Vty.EvKey (Vty.KChar 'e') [] ->
        --    let newList = BL.listInsert 0 (Types.Recipe 0 "test" "testy" False) l
        --    in B.continue $ CyanideState conn (RecipeSelectionScreen newList)

        Vty.EvKey (Vty.KEsc) [] -> B.continue $ CyanideState conn prev

        ev -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (RecipeDetailScreen r g is _)) =
    [ BC.center
        $ B.hLimit 80
        $ B.vLimit 25
        $ B.vBox [ BC.hCenter $ BB.border
                     $ B.vBox [ addPaddedRow 12 "Name" [B.txt $ Types.recipeName r]
                              , handleMaybeGlass g
                              , addPaddedRow 12 "Ingredients" (map B.txt $ map formatIngr is)
                              , addPaddedRow 12 "Instructions" [B.txt $ Types.instructions r]
                              ]
                 , renderInstructions [ ("Esc","Previous screen")
                                      , ("e","Edit recipe")
                                      ]
                 ]
    ]

    where handleMaybeGlass :: Maybe Types.Glass -> B.Widget Name
          handleMaybeGlass Nothing  = B.emptyWidget
          handleMaybeGlass (Just g) = addPaddedRow 12 "Glass" [B.txt $ Types.glassName g]
