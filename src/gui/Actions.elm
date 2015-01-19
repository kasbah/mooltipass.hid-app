module Actions where

-- Elm standard library
import Signal (..)
import GuiState (..)
import CommonState (..)

{-| The channel that user inputs can 'Signal.send' actions to that affect the
    GUI directly -}
guiActions : Channel Action
guiActions = channel NoOp

{-| The channel that user inputs can 'Signal.send' actions to that affect the common state with the background. These are sent to the background first before they bubble back up to the 'GuiState'. -}
commonActions : Channel CommonAction
commonActions = channel CommonNoOp
