{-# LANGUAGE OverloadedStrings #-}

module Cyanide.UI.RecipeDetailScreen where

import qualified Brick as B
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import Data.Monoid
import Control.Monad.IO.Class

import Cyanide.UI.State
import Cyanide.UI.Util
import qualified Cyanide.UI.RecipeInputScreen as RecipeInput
import qualified Cyanide.Data.Types as Types
import qualified Cyanide.Data.Recipes as Recipes
import qualified Cyanide.Data.Glasses as Glasses
import qualified Cyanide.Data.Postgres as Postgres

attrMap :: [(B.AttrName, Vty.Attr)]
attrMap = []

handleEvent :: CyanideState -> B.BrickEvent Name () -> B.EventM Name (B.Next CyanideState)
handleEvent s@(CyanideState conn _ scr@(RecipeDetailScreen r g is prev)) (B.VtyEvent e) =
    case e of
        Vty.EvKey (Vty.KChar 'e') [] -> do
            newScr <- liftIO $ RecipeInput.newRecipeInputScreen conn g is (recipeFor $ Types.recipeName r) (Just r) goBack 
            B.continue $ s { stateScreen = newScr }
          where goBack (Just (r,g,is)) = return $ RecipeDetailScreen r g is prev
                goBack Nothing = return $ scr

                recipeFor (Left _) = Nothing
                recipeFor (Right i) = Just i

        Vty.EvKey (Vty.KChar 'd') [Vty.MMeta] -> do
            B.continue $ s { stateScreen = RecipeDeletionScreen r goBack }
          where goBack True = prev Nothing
                goBack False = scr

        Vty.EvKey (Vty.KEsc) [] -> B.continue $ s { stateScreen = (prev $ Just (r,g,is)) }

        ev -> B.continue s
handleEvent s _ = B.continue s

drawUI :: CyanideState -> [B.Widget Name]
drawUI (CyanideState conn _ (RecipeDetailScreen r g is _)) =
    [ BC.center
        $ B.hLimit 80
        $ B.vLimit 25
        $ B.vBox [ B.vLimit 22
                    $ B.hBox [ B.hLimit 40 $ BB.borderWithLabel (B.txt "Info")
                                $ B.padBottom (B.Max)
                                $ B.padAll 1
                                $ B.vBox [ handleRecipeName $ Types.recipeName r
                                         , handleMaybeGlass g
                                         , handleGarnish $ Types.recipeGarnish r
                                         , addPaddedRow 7 "Parts" (map B.txtWrap $ map formatIngr is)
                                         ]
                             , BB.borderWithLabel (B.txt "Instructions")
                                $ B.padBottom (B.Max) $ B.padRight (B.Max)
                                $ B.padAll 1
                                $ B.txtWrap $ let instr = Types.instructions r
                                              in if instr == "" then " " else instr
                             ]
                 , renderInstructions [ ("Esc","Previous screen")
                                      , ("e","Edit recipe")
                                      , ("Alt-d","Delete recipe")
                                      ]
                 ]
    ]

    where handleMaybeGlass :: Maybe Types.Glass -> B.Widget Name
          handleMaybeGlass Nothing  = B.emptyWidget
          handleMaybeGlass (Just g) = addPaddedRow 7 "Glass" [B.txt $ Types.glassName g]

          handleGarnish :: T.Text -> B.Widget Name
          handleGarnish ""  = B.emptyWidget
          handleGarnish gn = addPaddedRow 7 "Garnish" [B.txtWrap gn]

          handleRecipeName :: Either T.Text Types.Ingredient -> B.Widget Name
          handleRecipeName (Left n) = addPaddedRow 7 "Name" [B.txtWrap $ n]
          handleRecipeName (Right i) = addPaddedRow 7 "Name" [B.txtWrap $ "recipe for " `T.append` Types.ingredientName i]
