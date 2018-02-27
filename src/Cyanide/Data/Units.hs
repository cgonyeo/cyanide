{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Units where

import Cyanide.Data.Types
import qualified Data.Text as T
import Control.Monad

calculateAvgPricePerOz :: [Purchase] -> Maybe Int
calculateAvgPricePerOz [] = Nothing
calculateAvgPricePerOz ps = do
    ounces <- forM ps convertToOz
    return $ sum (map price ps) `div` floor (sum (ounces))

-- convertToOz treats ounces as fluid ounces
convertToOz :: Purchase -> Maybe Double
convertToOz p = convertToOz' $ p { unit = T.toLower (unit p) }
        -- Base accepted units
  where convertToOz' p@(Purchase _ _ _ v "oz") = Just (fromIntegral v)
        convertToOz' p@(Purchase _ _ _ v "ml") = Just $ (fromIntegral v) / 29.57

        -- Metric conversions
        convertToOz' p@(Purchase _ _ _ v "cl") = convertToOz $ p { volume = 10 * v
                                                                 , unit = "ml"
                                                                 }
        convertToOz' p@(Purchase _ _ _ v "dl") = convertToOz $ p { volume = 100 * v
                                                                 , unit = "ml"
                                                                 }
        convertToOz' p@(Purchase _ _ _ v "l") = convertToOz $ p { volume = 1000 * v
                                                                , unit = "ml"
                                                                }
        convertToOz' p@(Purchase _ _ _ v "dal") = convertToOz $ p { volume = 10000 * v
                                                                  , unit = "ml"
                                                                  }
        convertToOz' p@(Purchase _ _ _ v "hl") = convertToOz $ p { volume = 100000 * v
                                                                 , unit = "ml"
                                                                 }
        convertToOz' p@(Purchase _ _ _ v "kl") = convertToOz $ p { volume = 1000000 * v
                                                                 , unit = "ml"
                                                                 }
        convertToOz' p@(Purchase _ _ _ v "millileter") = convertToOz $ p { unit = "ml" }
        convertToOz' p@(Purchase _ _ _ v "centileter") = convertToOz $ p { unit = "cl" }
        convertToOz' p@(Purchase _ _ _ v "leter") = convertToOz $ p { unit = "l" }

        -- Imperial conversions
        convertToOz' p@(Purchase _ _ _ v "oz.") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "ounce") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "ounces") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "fl oz") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "floz") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "fl. oz.") = convertToOz $ p { unit = "oz" }
        convertToOz' p@(Purchase _ _ _ v "gl") = convertToOz $ p { volume = 128 * v
                                                                 , unit = "oz"
                                                                 }
        convertToOz' p@(Purchase _ _ _ v "gallon") = convertToOz $ p { volume = 128 * v
                                                                     , unit = "oz"
                                                                     }

        -- Failed matches
        convertToOz' _ = Nothing
