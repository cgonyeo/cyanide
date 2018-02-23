{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.GlassInputScreen where

import Lens.Micro ((^.))
import qualified Brick as B
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
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
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn scr@(GlassInputScreen ed mg l)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KEsc) [] ->
            B.continue $ CyanideState conn (GlassSelectionScreen l)

        Vty.EvKey (Vty.KEnter) [] -> do
            let newGlassNames = BE.getEditContents ed
            if length newGlassNames /= 1
                then B.continue s
                else do
                    glasses <- liftIO $ Glasses.getGlasses conn
                    let newGlassName = newGlassNames !! 0
                    case (mg,filter (\(Types.Glass _ n) -> n == newGlassName) glasses) of
                        (Just (Types.Glass i _),[]) -> do
                            newGlass <- liftIO $ Glasses.updateGlass conn i newGlassName
                            let newList = BL.listModify (\_ -> newGlass) l
                            B.continue $ CyanideState conn (GlassSelectionScreen newList)
                        (Nothing,[]) -> do
                            newGlass <- liftIO $ Glasses.newGlass conn newGlassName
                            let newList = BL.listInsert (length l) newGlass l
                            B.continue $ CyanideState conn (GlassSelectionScreen newList)
                        _ -> B.continue s

        ev -> do
            newEdit <- BE.handleEditorEvent e ed
            B.continue $ CyanideState conn scr { glassCreationName = newEdit }
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn (GlassInputScreen e mg l)) = [ui]
    where prompt = case mg of
                    Just (Types.Glass _ n) -> "What do you want to rename \"" `T.append` n `T.append` "\" to?"
                    Nothing -> "What glass do you want to create?"


          enterAction = case mg of
                            Just _ -> "Modify"
                            Nothing -> "Create"
    
          ui = BC.center
               $ B.hLimit 80
               $ B.vLimit 25 $ B.vBox
                            [ BC.hCenter $ B.txt prompt
                            , BC.hCenter $ B.hLimit 24 $ B.padAll 1 $ BB.border $ BE.renderEditor drawEdit True e
                            , renderInstructions [ ("Enter",enterAction)
                                                 , ("Esc","Cancel")
                                                 ]
                            ]

drawEdit = B.txt . T.unlines
