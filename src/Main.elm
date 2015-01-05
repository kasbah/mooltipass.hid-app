import Graphics.Element (..)
import Signal (..)
import Window

import CustomGraphics (roundedRect)
import State (..)
import Scene (..)

main : Signal Element
main = scene <~ Window.dimensions
             ~ (foldp update defaultState (subscribe actions))

