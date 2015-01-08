module UserActions where

-- Elm standard library
import Signal (..)

-- local source
import State (..)

{-| The channel that user inputs can 'Signal.send' actions to -}
userActions : Channel Action
userActions = channel NoOp
