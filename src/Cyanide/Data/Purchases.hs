{-# LANGUAGE OverloadedStrings #-}

module Cyanide.Data.Purchases where

import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T

import Cyanide.Data.Types
import Cyanide.Data.Postgres
import Data.Time.Calendar

getPurchasesForIngredient :: DBConn -> Ingredient -> IO [Purchase]
getPurchasesForIngredient conn (Ingredient i _ _ _ _ _)=
    P.query conn  "SELECT purchases.date      \
                 \      , purchases.location  \
                 \      , purchases.price     \
                 \ FROM purchases             \
                 \ WHERE ingredient = ?       \
                 \ ORDER BY purchases.date DESC" (P.Only i)

newPurchase :: DBConn -> (Ingredient,Day,T.Text,Int) -> IO ()
newPurchase conn ((Ingredient i _ _ _ _ _),d,l,p) = do
    P.execute conn "INSERT INTO PURCHASES VALUES (?,?,?,?)" (i,d,l,p)
    return ()

deletePurchase :: DBConn -> Purchase -> IO ()
deletePurchase conn (Purchase t _ _) = do
    P.execute conn "DELETE FROM purchases WHERE date = ?" (P.Only t)
    return ()
