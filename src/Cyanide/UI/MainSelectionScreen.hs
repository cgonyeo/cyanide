{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.MainSelectionScreen where

import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick as B
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.UI.RecipeSelectionScreen as RecipeSelection
import qualified Cyanide.UI.RecipeSelectionFilterScreen as RecipeSelectionFilter
import qualified Cyanide.UI.IngredientSelectionScreen as IngredientSelection
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Postgres as Postgres

getRecipeSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getRecipeSelectionScreen conn = RecipeSelection.newRecipeSelectionScreen conn

getGlassSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getGlassSelectionScreen conn = do
    glasses <- Glasses.getGlasses conn
    return $ GlassSelectionScreen $ BL.list "GlassSelectionScreen" (V.fromList glasses) 1

getIngredientClassSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getIngredientClassSelectionScreen conn = do
    ingredientClasses <- IngredientClasses.getIngredientClasses conn
    return $ IngredientClassSelectionScreen $ BL.list "IngredientClassSelectionScreen" (V.fromList ingredientClasses) 1

getIngredientSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getIngredientSelectionScreen conn = IngredientSelection.newIngredientSelectionScreen conn

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ MainSelectionScreen) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KChar 'r') [] -> do
            scr <- liftIO $ getRecipeSelectionScreen conn
            B.continue $ s { stateScreen = scr }

        Vty.EvKey (Vty.KChar 'g') [] -> do
            scr <- liftIO $ getGlassSelectionScreen conn
            B.continue $ s { stateScreen = scr }

        Vty.EvKey (Vty.KChar 'c') [] -> do
            scr <- liftIO $ getIngredientClassSelectionScreen conn
            B.continue $ s { stateScreen = scr }

        Vty.EvKey (Vty.KChar 'i') [] -> do
            scr <- liftIO $ getIngredientSelectionScreen conn
            B.continue $ s { stateScreen = scr }

        Vty.EvKey Vty.KEsc [] -> B.halt s

        _ -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ MainSelectionScreen) = 
    [ BC.center
        $ B.hLimit 80
        $ B.vLimit 25
        $ B.hBox [ bottle1
                 , B.txt "   "
                 , B.hLimit 34
                        $ B.vBox [ BC.hCenter $ B.txt "Cyanide: home bar management system"
                                 , BC.hCenter $ BB.hBorder
                                 , renderInstructions [ ("r","Recipes")
                                                      , ("i","Ingredients")
                                                      , ("g","Glasses")
                                                      , ("c","Ingredient classes")
                                                      , ("Esc","Exit")
                                                      ]
                                 ]
                 , B.txt "   "
                 , bottle1
                 ]
    ]
  where bottle1 = B.vBox
            [ B.txt "  [~]  "
            , B.txt "  |=|  "
            , B.txt ".-' '-."
            , B.txt "|-----|"
            , B.txt "| ~~~ |"
            , B.txt "| ~~~ |"
            , B.txt "| XXX |"
            , B.txt "|-----|"
            , B.txt "'-----'"
            ]
        bottle2 = B.vBox
            [ B.txt "  _                 "
            , B.txt " {_}                "
            , B.txt " |(|                "
            , B.txt " |=|                "
            , B.txt "/   \\               "
            , B.txt "|.--|               "
            , B.txt "||  |               "
            , B.txt "||  |    .    ' .   "
            , B.txt "|'--|  '     \\~~~/  "
            , B.txt "'-=-' \\~~~/   \\_/   "
            , B.txt "       \\_/     Y    "
            , B.txt "        Y     _|_   "
            , B.txt "       _|_          "
            ]
