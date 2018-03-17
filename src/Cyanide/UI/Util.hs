{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.Util where

import qualified Brick as B
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Focus as BF
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Read as TR

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
formatMoney n = "$" `T.append` (T.pack $ show $ (fromIntegral n / 100 :: Double))

addRow :: Int -> T.Text -> [B.Widget Name] -> B.Widget Name
addRow maxTitleSize title contents =
  B.hBox [ formatText JustifyRight maxTitleSize title
         , B.padLeft (B.Pad 2) $ B.vBox contents
         ]

addPaddedRow :: Int -> T.Text -> [B.Widget Name] -> B.Widget Name
addPaddedRow maxTitleSize title contents =
  B.padBottom (B.Pad 1) $ addRow maxTitleSize title contents

(+++) :: T.Text -> T.Text -> T.Text
(+++) t1 "" = t1
(+++) "" t2 = t2
(+++) t1 t2 = t1 `T.append` " " `T.append` t2

displayIng :: Either Types.Ingredient Types.IngredientClass -> T.Text
displayIng (Left (Types.Ingredient _ n _ _ _)) = n
displayIng (Right (Types.IngredientClass _ n)) = n

formatIngr :: Types.IngredientListItem -> T.Text
formatIngr (Types.IngredientListItem num den unit e) = fractionToText (Fraction num den) +++ unit +++ displayIng e
    where fractionToText (Fraction 0 1) = ""
          fractionToText f = T.pack $ show f

displayAmt :: Int -> Int -> T.Text -> T.Text
displayAmt num 1 unit = (T.pack $ show num) +++ unit
displayAmt num den unit = (T.pack $ show num) `T.append` "/" `T.append` (T.pack $ show den) +++ unit

renderInstructions :: [(T.Text,T.Text)] -> B.Widget Name
renderInstructions lst = BC.hCenter $ B.vBox $ map (\(key,label) -> B.hBox [ formatText JustifyRight maxInput key, B.txt " - ", B.txt label]) lst
    where maxInput = maximum $ map (T.length . fst) lst

getEditorLine :: BE.Editor T.Text n -> Maybe T.Text
getEditorLine ed = do
    let ls = BE.getEditContents ed
    if length ls /= 1
        then Nothing
        else Just $ ls !! 0

data Fraction = Fraction Int Int

instance Show Fraction where
    show (Fraction num den)
            | den == 0 = "undefined"
            | den == 1 = show num
            | num == den = "1"
            | num < den = show num ++ "/" ++ show den
            | num `mod` den == 0 = show (num `div` den)
            | otherwise = show (num `div` den) ++ " " ++ show (num `mod` den) ++ "/" ++ show den


readFraction :: String -> Maybe Fraction
readFraction str
    | L.isSubsequenceOf " " str && L.isSubsequenceOf "/" str= do
        let (p1,(_:p2)) = L.break (==' ') str
        partOfNum <- TR.readMaybe p1
        (Fraction n d) <- readFraction p2
        return $ Fraction ((partOfNum * d) + n) d
    | L.isSubsequenceOf "/" str = do
        let (p1,(_:p2)) = L.break (=='/') str
        n <- TR.readMaybe p1
        d <- TR.readMaybe p2
        return $ Fraction n d
    | str == "" = Just (Fraction 0 1)
    | otherwise = do
        n <- TR.readMaybe str
        return $ Fraction n 1

setFocusTo :: BF.FocusRing Name -> Name -> BF.FocusRing Name
setFocusTo f n =
    let firstFocus = BF.focusGetCurrent f
    in setFocusTo' firstFocus (BF.focusNext f) n
  where setFocusTo' :: Maybe Name -> BF.FocusRing Name -> Name -> BF.FocusRing Name
        setFocusTo' firstFocus f n =
            case BF.focusGetCurrent f of
                Nothing -> f
                (Just n')
                    | Just n' == firstFocus -> f
                    | n' == n               -> f
                    | otherwise             -> setFocusTo' firstFocus (BF.focusNext f) n

globalWidth :: Int
globalWidth = 80

globalHeight :: Int
globalHeight = 25

addGlobalUI :: CyanideState -> [B.Widget Name] -> [B.Widget Name]
addGlobalUI s uis = map drawGlobalUI uis
    where drawGlobalUI ui = BC.center
                                $ B.hLimit globalWidth
                                $ B.vLimit globalHeight
                                $ B.vBox [ getBreadcrumbsForScreen (stateScreen s)
                                         , BC.center $ ui
                                         ]
