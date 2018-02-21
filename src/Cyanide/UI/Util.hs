{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.Util where

import qualified Brick as B
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Edit as BE
import qualified Data.Text as T

import Cyanide.UI.State
import qualified Cyanide.Data.Types as Types

data Justify = JustifyLeft
             | Center
             | JustifyRight

formatText :: Justify -> Int -> T.Text -> B.Widget Name
formatText JustifyLeft  size t = B.hLimit size $ B.txt $ T.justifyLeft  size ' ' t
formatText Center       size t = B.hLimit size $ B.txt $ T.center       size ' ' t
formatText JustifyRight size t = B.hLimit size $ B.txt $ T.justifyRight size ' ' t

formatMoney :: Int -> T.Text
formatMoney n = "$" `T.append` (T.pack $ show $ fromIntegral n / 100)

addRow :: Int -> T.Text -> [B.Widget Name] -> B.Widget Name
addRow maxTitleSize title contents =
  B.hBox [ formatText JustifyRight maxTitleSize title
         , B.padLeft (B.Pad 2) $ B.vBox contents
         ]

addPaddedRow :: Int -> T.Text -> [B.Widget Name] -> B.Widget Name
addPaddedRow maxTitleSize title contents =
  B.padAll 1 $ addRow maxTitleSize title contents

(+++) :: T.Text -> T.Text -> T.Text
(+++) t1 t2 = t1 `T.append` " " `T.append` t2
                                                                      
displayIng :: Either Types.Ingredient Types.IngredientClass -> T.Text
displayIng (Left (Types.Ingredient _ n _ _ _ _)) = n
displayIng (Right (Types.IngredientClass _ n)) = n
                                                                      
formatIngr :: Types.IngredientListItem -> T.Text
formatIngr (Types.IngredientListItem num den unit e) = displayAmt num den unit +++ displayIng e

displayAmt :: Int -> Int -> T.Text -> T.Text
displayAmt num 1 unit = (T.pack $ show num) +++ unit
displayAmt num den unit = (T.pack $ show num) `T.append` "/" `T.append` (T.pack $ show den) +++ unit

renderInstructions :: [(T.Text,T.Text)] -> B.Widget Name
renderInstructions lst = BC.hCenter $ B.vBox $ map (\(key,label) -> B.hBox [ formatText JustifyRight maxInput key, B.txt " - ", B.txt label]) lst
    where maxInput = maximum $ map (T.length . fst) lst

getEditorLine :: BE.Editor T.Text n -> Maybe T.Text
getEditorLine ed = do
    let lines = BE.getEditContents ed
    if length lines /= 1
        then Nothing
        else Just $ lines !! 0
