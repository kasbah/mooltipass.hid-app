module Scene where

-- Elm standard library
import Graphics.Input (..)
import Graphics.Element (..)
import Signal (..)

-- local source
import State (..)
import Navigation (..)
import Layout (..)
import Content (..)

{-| The scene maps the window dimensions and an application state to the main
    'Element'-}
scene : (Int,Int) -> State -> Element
scene dims state = layers [layer1 dims state]

{-| This is currently the only layer, might be useful to easily add layers to
    the scene though -}
layer1 : (Int, Int) -> State -> Element
layer1 dims state =
    flow down [ spacer 1 heights.marginTop
              , navigation dims state
              , content dims state
              ]
