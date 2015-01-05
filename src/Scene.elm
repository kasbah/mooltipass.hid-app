module Scene where
import Graphics.Input (..)
import Graphics.Element (..)
import Signal (..)

import State (..)
import Navigation (..)
import Layout (..)
import Content (..)

scene : (Int,Int) -> State -> Element
scene dims state =
    clickable (send actions (AppendToLog ("\n> " ++ toString dims)))
        <| layers [layer1 dims state]

layer1 : (Int, Int) -> State -> Element
layer1 dims state =
    flow down [ spacer 1 heights.marginTop
              , navigation dims state
              , content dims state
              ]

