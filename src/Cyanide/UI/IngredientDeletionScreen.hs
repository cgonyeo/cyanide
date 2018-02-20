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
handleEvent s@(CyanideState conn (IngredientDeletionScreen usedIn mr l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn (IngredientSelectionScreen l)

        Vty.EvKey (Vty.KChar 'n') [] ->
            B.continue $ CyanideState conn (IngredientSelectionScreen l)

        Vty.EvKey (Vty.KChar 'y') [] ->
            case (length usedIn,mr) of
                (0,Nothing) -> deleteIngredient l
                (0,Just r) -> do liftIO $ Recipes.deleteRecipe conn r
                                 deleteIngredient l

        _ -> B.continue s
    where deleteIngredient l = do
            let Just (i,ingr) = BL.listSelectedElement l
                newList = BL.listRemove i l
            liftIO $ Ingredients.deleteIngredient conn ingr
            B.continue $ CyanideState conn (IngredientSelectionScreen newList)
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (IngredientDeletionScreen usedIn mr l)) = [ui]
    where Just (_,(Types.Ingredient _ n _ _ _ _)) = BL.listSelectedElement l
          uiContent =
            case (length usedIn,mr) of
                (0,Nothing) -> 
                    B.vBox [ BC.hCenter $ B.txt "Are you sure you want to delete the following ingredient?"
                           , BC.hCenter $ B.padAll 1 $ B.txt n
                           , renderInstructions [ ("y","Yes")
                                                , ("n","No")
                                                ]
                           ]
                (0,Just r) ->
                    B.vBox [ BC.hCenter $ B.txt "Are you sure you want to delete the following ingredient?"
                           , BC.hCenter $ B.padAll 1 $ B.txt n
                           , BC.hCenter $ B.padBottom (B.Pad 1) $ B.txt "This will delete the recipe for this ingredient too!"
                           , renderInstructions [ ("y","Yes")
                                                , ("n","No")
                                                ]
                           ]
                (_,_) ->
                    B.vBox [ BC.hCenter $ B.txt "The following ingredient cannot be deleted, it's still used in recipes!"
                           , BC.hCenter $ B.padAll 1 $ B.txt n
                           , renderInstructions [ ("Esc","Previous screen")
                                                ]
                           ]

          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 uiContent
