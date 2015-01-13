module Actions where

-- Elm standard library
import Signal (..)
import GuiState (..)
import CommonState (..)

{-| The channel that user inputs can 'Signal.send' actions to -}
guiActions : Channel Action
guiActions = channel NoOp

commonActions : Channel CommonAction
commonActions = channel CommonNoOp
