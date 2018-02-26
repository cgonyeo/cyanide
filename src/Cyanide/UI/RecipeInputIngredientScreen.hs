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
import Text.Read
import Data.Maybe
import qualified Data.List as L
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
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
handleEvent s@(CyanideState conn _ scr@(RecipeInputIngredientScreen rname amountEd unitEd filterEd ingrListOrig ingrList f goBack)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            let newScr = goBack Nothing
            in B.continue $ s { stateScreen = newScr }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { recipeInputIngredientFocusRing = newFocus } }

        Vty.EvKey Vty.KEnter [] -> do
            case parseAmount $ fromJust $ getEditorLine amountEd of
                Nothing -> B.continue s
                Just (amtNum,amtDen) -> do
                    let unitInput = unitAliases $ fromJust $ getEditorLine unitEd

                    case BL.listSelectedElement ingrList of
                        Nothing -> B.continue s
                        Just (_,i) -> do
                            ingrItem <- liftIO $ makeIngrItem conn amtNum amtDen unitInput i
                            let newScr = goBack (Just ingrItem)
                            B.continue $ s { stateScreen = newScr }


        ev -> if BF.focusGetCurrent (f) == Just amountName then do
                    newEdit <- BE.handleEditorEvent ev amountEd
                    B.continue $ s { stateScreen = scr { recipeInputIngredientAmount = newEdit } }
              else if BF.focusGetCurrent (f) == Just unitName then do
                    newEdit <- BE.handleEditorEvent ev unitEd
                    B.continue $ s { stateScreen = scr { recipeInputIngredientUnit = newEdit } }
              else if BF.focusGetCurrent (f) == Just ingrListName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev ingrList
                    B.continue $ s { stateScreen = scr { recipeInputIngredientOptionsList = newList } }
              else if BF.focusGetCurrent (f) == Just filterName then do
                    -- Update the editor
                    newEdit <- BE.handleEditorEvent ev filterEd
                    -- Apply the new filter
                    let filteredList = case getEditorLine newEdit of
                                            (Just text) -> filter (filterFunc text) ingrListOrig
                                            Nothing -> ingrListOrig
                    let newList = BL.listReplace (V.fromList filteredList) (Just 0) ingrList
                    B.continue $ s { stateScreen = scr { recipeInputIngredientFilter = newEdit
                                                       , recipeInputIngredientOptionsList = newList
                                                       } }
              else B.continue s

  where filterFunc filterText i = L.isInfixOf (T.unpack $ T.toLower filterText) (T.unpack $ T.toLower (getListItemName i))

        parseAmount :: T.Text -> Maybe (Int,Int)
        parseAmount x = case readFraction $ T.unpack x of
                                (Just (Fraction n d)) -> Just (n,d)
                                Nothing -> Nothing

        unitAliases :: T.Text -> T.Text
        unitAliases x = case T.toLower x of
                            "oz" -> "oz"
                            "ounce" -> "oz"
                            "ounces" -> "oz"
                            "ml" -> "mL"
                            "millileter" -> "mL"
                            "teaspoon" -> "tsp"
                            "tsp" -> "tsp"
                            "tablespoon" -> "tbsp"
                            "tbsp" -> "tbsp"
                            "dash" -> "dash"
                            "splash" -> "splash"
                            "sprig" -> "sprig"
                            s -> s

        makeIngrItem :: Postgres.DBConn -> Int -> Int -> T.Text -> RecipeInputIngrListItem -> IO Types.IngredientListItem
        makeIngrItem conn num den unit (IngredientListItem i _) = do
            ingr <- Ingredients.getIngredient conn i
            return $ Types.IngredientListItem num den unit (Left ingr)
        makeIngrItem conn num den unit (IngredientClassListItem i _) = do
            ic <- IngredientClasses.getIngredientClass conn i
            return $ Types.IngredientListItem num den unit (Right ic)


handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (RecipeInputIngredientScreen rname amountEd unitEd filterEd _ ingrList f goBack)) = [ui]
    where amountRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) amountEd
          unitRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) unitEd
          filterRenderedEd = BF.withFocusRing f (BE.renderEditor drawEdit) filterEd
          ingrRenderedLst = BF.withFocusRing f (BL.renderList drawList) ingrList

          prompt = "Adding an ingredient to " `T.append` rname

          leftColumn = BC.vCenter
                        $ B.vBox [ BC.hCenter $ B.txt "Amount"
                                 , BC.hCenter $ B.hLimit 12 $ BB.border amountRenderedEd
                                 , BC.hCenter $ B.txt "(must be an integer or fraction)"
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
                                                 , ("Tab","Change focus")
                                                 , ("Esc","Cancel")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines

drawList :: Bool -> RecipeInputIngrListItem -> B.Widget Name
drawList True (IngredientListItem _ n) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"
drawList True (IngredientClassListItem _ n) = BC.hCenter $ B.txt $ "* " `T.append` n `T.append` " *"
drawList False (IngredientListItem _ n) = BC.hCenter $ B.txt n
drawList False (IngredientClassListItem _ n) = BC.hCenter $ B.txt n
