module Actions where

-- Elm standard library
import Signal (..)

-- local source
import GuiState (..)

{-| The channel that user inputs can 'Signal.send' actions to -}
guiActions : Channel Action
guiActions = channel NoOp
