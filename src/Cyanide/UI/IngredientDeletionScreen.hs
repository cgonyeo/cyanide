{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientDeletionScreen where

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
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ (IngredientDeletionScreen ingr usedIn mr prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = prev False }

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ s { stateScreen = prev False }

        Vty.EvKey (Vty.KChar 'y') [] ->
            case (length usedIn,mr) of
                (0,Nothing) -> deleteIngredient
                (0,Just r) -> do liftIO $ Recipes.deleteRecipe conn r
                                 deleteIngredient
                _ -> B.continue s

        _ -> B.continue s
    where deleteIngredient = do
            liftIO $ Ingredients.deleteIngredient conn ingr
            B.continue $ s { stateScreen = prev True }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientDeletionScreen ingr usedIn mr _)) = [ui]
    where ui =
            case (length usedIn,mr) of
                (0,Nothing) -> 
                    B.vBox [ BC.hCenter $ B.txt "Are you sure you want to delete the following ingredient?"
                           , BC.hCenter $ B.padAll 1 $ B.txt (Types.ingredientName ingr)
                           , renderInstructions [ ("y","Yes")
                                                , ("n","No")
                                                ]
                           ]
                (0,Just r) ->
                    B.vBox [ BC.hCenter $ B.txt "Are you sure you want to delete the following ingredient?"
                           , BC.hCenter $ B.padAll 1 $ B.txt (Types.ingredientName ingr)
                           , BC.hCenter $ B.padBottom (B.Pad 1) $ B.txt "This will delete the recipe for this ingredient too!"
                           , renderInstructions [ ("y","Yes")
                                                , ("n","No")
                                                ]
                           ]
                (_,_) ->
                    B.vBox [ BC.hCenter $ B.txt "The following ingredient cannot be deleted, it's still used in recipes!"
                           , BC.hCenter $ B.padAll 1 $ B.txt (Types.ingredientName ingr)
                           , renderInstructions [ ("Esc","Previous screen")
                                                ]
                           ]
