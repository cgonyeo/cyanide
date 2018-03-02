{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Purchases where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Data.Time.Calendar
import Control.Monad

getPurchasesForIngredient :: DBConn -> Ingredient -> IO [Purchase]
getPurchasesForIngredient conn i =
    P.query conn  "SELECT purchases.date      \
                 \      , purchases.location  \
                 \      , purchases.price     \
                 \      , purchases.volume    \
                 \      , purchases.unit      \
                 \ FROM purchases             \
                 \ WHERE ingredient = ?       \
                 \ ORDER BY purchases.date DESC" (P.Only $ ingredientId i)

newPurchase :: DBConn -> (Ingredient,Day,T.Text,Int,Int,T.Text) -> IO ()
newPurchase conn (i,d,l,p,a,u) =
    void $ P.execute conn "INSERT INTO PURCHASES VALUES (?,?,?,?,?,?)" (ingredientId i,d,l,p,a,u)

deletePurchase :: DBConn -> Purchase -> IO ()
deletePurchase conn p =
    void $ P.execute conn "DELETE FROM purchases WHERE date = ?" (P.Only $ date p)
