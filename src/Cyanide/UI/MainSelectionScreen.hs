{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.MainSelectionScreen where

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
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Postgres as Postgres

getRecipeSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getRecipeSelectionScreen conn = do
    recipes <- Recipes.getRecipes conn
    return $ RecipeSelectionScreen $ BL.list "RecipeSelectionScreen" (V.fromList recipes) 1

getGlassSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getGlassSelectionScreen conn = do
    glasses <- Glasses.getGlasses conn
    return $ GlassSelectionScreen $ BL.list "GlassSelectionScreen" (V.fromList glasses) 1

getIngredientClassSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getIngredientClassSelectionScreen conn = do
    ingredientClasses <- IngredientClasses.getIngredientClasses conn
    return $ IngredientClassSelectionScreen $ BL.list "IngredientClassSelectionScreen" (V.fromList ingredientClasses) 1

getIngredientSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
getIngredientSelectionScreen conn = do
    ingredients <- Ingredients.getIngredients conn
    return $ IngredientSelectionScreen $ BL.list "IngredientSelectionScreen" (V.fromList ingredients) 1

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn MainSelectionScreen) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KChar 'r') [] -> do
            scr <- liftIO $ getRecipeSelectionScreen conn
            B.continue $ CyanideState conn scr

        Vty.EvKey (Vty.KChar 'g') [] -> do
            scr <- liftIO $ getGlassSelectionScreen conn
            B.continue $ CyanideState conn scr

        Vty.EvKey (Vty.KChar 'c') [] -> do
            scr <- liftIO $ getIngredientClassSelectionScreen conn
            B.continue $ CyanideState conn scr

        Vty.EvKey (Vty.KChar 'i') [] -> do
            scr <- liftIO $ getIngredientSelectionScreen conn
            B.continue $ CyanideState conn scr

        Vty.EvKey Vty.KEsc [] -> B.halt s

        _ -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn MainSelectionScreen) = 
    [ BC.center
        $ B.hLimit 80
        $ B.vLimit 25
        $ B.vBox [ BC.hCenter $ B.txt "Cyanide: home bar management system"
                 , BC.hCenter $ B.hLimit 34 BB.hBorder
                 , renderInstructions [ ("r","Recipes")
                                      , ("g","Glasses")
                                      , ("i","Ingredients")
                                      , ("c","Ingredient classes")
                                      , ("Esc","Exit")
                                      ]
                 ]
    ]
