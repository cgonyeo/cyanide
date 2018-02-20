{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeInputIngredientScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Focus as BF
import Data.Monoid
import Data.Maybe
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Postgres as Postgres

amountName :: Name
amountName = "RecipeInputIngredientAmount"

unitName :: Name
unitName = "RecipeInputIngredientUnit"

filterName :: Name
filterName = "RecipeInputIngredientFilter"

ingrListName :: Name
ingrListName = "RecipeInputIngredientList"

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(RecipeInputIngredientScreen rname amountEd unitEd filterEd ingrListOrig ingrList f goBack)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            let newScr = goBack Nothing
            in B.continue $ CyanideState conn newScr

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ CyanideState conn $ scr { recipeInputIngredientFocusRing = newFocus }

        ev -> if BF.focusGetCurrent (f) == Just amountName then do
                    newEdit <- BE.handleEditorEvent ev amountEd
                    B.continue $ CyanideState conn $ scr { recipeInputIngredientAmount = newEdit }
              else if BF.focusGetCurrent (f) == Just unitName then do
                    newEdit <- BE.handleEditorEvent ev unitEd
                    B.continue $ CyanideState conn $ scr { recipeInputIngredientUnit = newEdit }
              else if BF.focusGetCurrent (f) == Just filterName then do
                    newEdit <- BE.handleEditorEvent ev filterEd
                    B.continue $ CyanideState conn $ scr { recipeInputIngredientFilter = newEdit }
              else B.continue s

  --where getAndCheckEditorName mustBeUnique = do
  --          let newIngredientNames = BE.getEditContents ed
  --          if length newIngredientNames /= 1
  --              then return Nothing
  --              else do
  --                  ingredients <- liftIO $ Ingredients.getIngredients conn
  --                  let newIngredientName = newIngredientNames !! 0
  --                  if not mustBeUnique
  --                      then return $ Just newIngredientName
  --                      else case filter (\i -> Types.ingredientName i == newIngredientName) ingredients of
  --                              [] -> return $ Just newIngredientName
  --                              _ -> return Nothing

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (RecipeInputIngredientScreen rname amountEd unitEd filterEd _ ingrList f goBack)) = [ui]
    where amountRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) amountEd
          unitRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) unitEd
          filterRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) filterEd
          ingrRenderedLst = BF.withFocusRing f (BL.renderList drawList) ingrList

          prompt = "Adding an ingredient to \"" `T.append` rname `T.append` "\""

          leftColumn = BC.vCenter
                        $ B.vBox [ BC.hCenter $ B.txt "Amount"
                                 , BC.hCenter $ B.hLimit 12 $ BB.border amountRenderedEd
                                 , B.txt " "
                                 , BC.hCenter $ B.txt "Unit"
                                 , BC.hCenter $ B.hLimit 12 $ BB.border unitRenderedEd
                                 ]

          rightColumn = B.vBox [ B.hBox [ B.txt "Filter: "
                                        , filterRenderedEd
                                        ]
                               , BB.border ingrRenderedLst
                               ]

          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt prompt
                            , B.hBox [ leftColumn
                                     , rightColumn
                                     ]
                            , renderInstructions [ ("Enter","Add ingredient")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines

drawList :: Bool -> RecipeInputIngrListItem -> B.Widget Name
drawList _ (IngredientListItem _ n) = BC.hCenter $ B.txt n
drawList _ (IngredientClassListItem _ n) = BC.hCenter $ B.txt n
