{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeSelectionFilterScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.UI.RecipeInputScreen as RecipeInput
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.Postgres as Postgres

ingredientClassesName :: Name
ingredientClassesName = "RecipeSelectionFilterIngredientClasses"

glassesName :: Name
glassesName = "RecipeSelectionFilterGlasses"

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(RecipeSelectionFilterScreen rlf@(RecipeListFilter avail icl gl) f prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] -> do
            newScr <- liftIO $ prev Nothing
            B.continue $ s { stateScreen = newScr }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { recipeFilterFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar 'a') [] ->
            B.continue $ s { stateScreen = scr { recipeFilter = rlf { onlyWithAvailIngredients = not avail } } }

        Vty.EvKey (Vty.KEnter) [] -> do
            newScr <- liftIO $ prev (Just rlf)
            B.continue $ s { stateScreen = newScr }

        ev -> if BF.focusGetCurrent (f) == Just ingredientClassesName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev icl
                    B.continue $ s { stateScreen = scr { recipeFilter = rlf { limitToIngredientClass = newList } } }
              else if BF.focusGetCurrent (f) == Just glassesName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev gl
                    B.continue $ s { stateScreen = scr { recipeFilter = rlf { limitToGlass = newList } } }
              else B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (RecipeSelectionFilterScreen rlf@(RecipeListFilter avail icl gl) f prev)) = [ui]
    where icList = BF.withFocusRing f (BL.renderList drawListIngredientClass) icl
          gList  = BF.withFocusRing f (BL.renderList drawListGlass) gl

          limitedToAvail = if avail then "Only showing recipes that can be made with available ingredients"
                                    else "Showing all recipes"

          ui = B.vBox [ BC.hCenter $ B.txt "Recipe Filter"
                      , B.padAll 1 $ BC.hCenter $ B.txt limitedToAvail
                      , B.hBox [ BB.borderWithLabel (B.txt "Ingredient Classes") icList
                               , BB.borderWithLabel (B.txt "Glasses") gList
                               ]
                      , renderInstructions [ ("Enter","Apply filter")
                                           , ("Tab","Change focus")
                                           , ("a","Toggle limiting to available ingredients")
                                           , ("Esc","Cancel")
                                           ]
                      ]

drawListIngredientClass :: Bool -> Maybe Types.IngredientClass -> B.Widget Name
drawListIngredientClass False Nothing = BC.hCenter $ B.txt " "
drawListIngredientClass False (Just (Types.IngredientClass _ n)) = BC.hCenter $ B.txt n
drawListIngredientClass True Nothing = BC.hCenter $ B.txt "*"
drawListIngredientClass True (Just (Types.IngredientClass _ n)) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"

drawListGlass :: Bool -> Maybe Types.Glass -> B.Widget Name
drawListGlass False Nothing = BC.hCenter $ B.txt " "
drawListGlass False (Just (Types.Glass _ n)) = BC.hCenter $ B.txt n
drawListGlass True Nothing = BC.hCenter $ B.txt "*"
drawListGlass True (Just (Types.Glass _ n)) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"
