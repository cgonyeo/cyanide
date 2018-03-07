{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.IngredientSelectionScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.List as L

import Cyanide.UI.State
import qualified Cyanide.UI.IngredientDetailScreen as IngredientDetail
import qualified Cyanide.UI.IngredientInputScreen as IngredientInput
import qualified Cyanide.Data.IngredientClasses as IngredientClasses
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Ingredients as Ingredients
import qualified Cyanide.Data.Purchases as Purchases
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Postgres as Postgres
import qualified Cyanide.Data.Units as Units
import Cyanide.UI.Util

ingredientsName :: Name
ingredientsName = "IngredientSelectionIngredientsList"

searchName :: Name
searchName = "IngredientSelectionSearchEditor"

newIngredientSelectionScreen :: Postgres.DBConn -> IO CyanideScreen
newIngredientSelectionScreen conn = do
    ingredients <- Ingredients.getIngredients conn
    let ingList = BL.list ingredientsName (V.fromList ingredients) 1
        se = BE.editorText searchName (Just 1) ""
        f = BF.focusRing [ ingredientsName, searchName ]
    return $ IngredientSelectionScreen ingList ingredients se f

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(IngredientSelectionScreen l orig se f)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ s { stateScreen = MainSelectionScreen }

        Vty.EvKey (Vty.KChar '\t') [] ->
            let newFocus = BF.focusNext f
            in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }

        Vty.EvKey (Vty.KChar '/') [] ->
            if BF.focusGetCurrent (f) == Just ingredientsName
                then let newFocus = BF.focusNext f
                     in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }
                else B.continue s

        Vty.EvKey Vty.KEnter [] ->
            if BF.focusGetCurrent (f) == Just ingredientsName then do
                let Just (j,ingr) = BL.listSelectedElement l
                newScr <- liftIO $ IngredientDetail.newIngredientDetailScreen conn ingr (goBack j)
                B.continue $ s { stateScreen = newScr }
            else if BF.focusGetCurrent (f) == Just searchName then
                let newFocus = BF.focusNext f
                in B.continue $ s { stateScreen = scr { ingredientListFocusRing = newFocus } }
            else B.continue s
          where goBack n (Just i) = let newList = BL.listModify (\_ -> i) l
                                        newOrig = map (\i' -> if Types.ingredientId i == Types.ingredientId i' then i else i') orig
                                    in scr { ingredientSelectionList = newList
                                           , ingredientListOrig = newOrig
                                           }
                goBack n Nothing = let newList = BL.listRemove n l
                                       Just (_,ingr) = BL.listSelectedElement l
                                       newOrig = filter (\i' -> Types.ingredientId i' /= Types.ingredientId ingr) orig
                                   in scr { ingredientSelectionList = newList
                                          , ingredientListOrig = newOrig
                                          }

        Vty.EvKey (Vty.KChar 'n') [Vty.MMeta] -> do
            ics <- liftIO $ IngredientClasses.getIngredientClasses conn

            let ed = BE.editor IngredientInput.editorName (Just 1) ""
                iclist = BL.list IngredientInput.classesName (V.fromList $ [Nothing] ++ map Just ics) 1
                f = BF.focusRing [ IngredientInput.editorName
                                 , IngredientInput.classesName
                                 ]
            B.continue $ s { stateScreen = IngredientInputScreen ed iclist f False Nothing (return . goBack) }
          where goBack Nothing = scr
                goBack (Just (i,_)) =
                    let newOrig = L.sortBy (\x y -> let f = T.toLower . Types.ingredientName in compare (f x) (f y)) (i:orig)
                        [(index,_)] = filter ((==i) . snd) $ zip [0..] newOrig
                        newList = BL.listReplace (V.fromList newOrig) (Just index) l
                        newEdit = BE.editorText searchName (Just 1) ""
                        newFocus = setFocusTo f ingredientsName
                    in scr { ingredientSelectionList = newList
                           , ingredientListOrig = newOrig
                           , ingredientListSearch = newEdit
                           , ingredientListFocusRing = newFocus
                           }

        ev -> if BF.focusGetCurrent (f) == Just ingredientsName then do
                    newList <- BL.handleListEventVi BL.handleListEvent ev l
                    B.continue $ s { stateScreen = scr { ingredientSelectionList = newList } }
              else if BF.focusGetCurrent (f) == Just searchName then do
                    newEdit <- BE.handleEditorEvent ev se
                    let filteredRecipes = filterOutSearch (getEditorLine newEdit) orig
                        newList = BL.list ingredientsName (V.fromList filteredRecipes) 1
                    B.continue $ s { stateScreen = scr { ingredientListSearch = newEdit
                                                       , ingredientSelectionList = newList
                                                       } }
              else B.continue s
          where filterOutSearch :: Maybe T.Text -> [Types.Ingredient] -> [Types.Ingredient]
                filterOutSearch (Just search) is = filter (filterFunc search) is
                filterOutSearch Nothing is = is

                filterFunc filterText ing = L.isInfixOf (T.unpack $ T.toLower filterText) (T.unpack $ T.toLower (Types.ingredientName ing))

handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (IngredientSelectionScreen l _ se f)) = [ui]
    where editor = BF.withFocusRing f (BE.renderEditor drawEdit) se
          ingredientList = BF.withFocusRing f (BL.renderList listDrawElement) l
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BB.borderWithLabel (B.txt "Search") editor
                            , BB.borderWithLabel (B.txt "Ingredients") ingredientList
                            , renderInstructions [ ("Enter","View details")
                                                 , ("Alt-n","New ingredient")
                                                 , ("Esc","Previous screen")
                                                 ]
                            ]

listDrawElement :: Bool -> Types.Ingredient -> B.Widget Name
listDrawElement sel (Types.Ingredient _ n mic a _) =
    BC.hCenter $ B.hBox [ formatText JustifyLeft 46 n
                        , formatText JustifyLeft 18 $ case mic of
                                                        Just (Types.IngredientClass _ n) -> n
                                                        _ -> ""
                        , formatText JustifyRight 12 (if a then "Available" else "")
                        ]

drawEdit = B.txt . T.unlines
