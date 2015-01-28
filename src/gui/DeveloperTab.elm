module DeveloperTab where

import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Signal
import Text (..)

import Color
import Layout (..)
import CustomGraphics (..)
import Actions (..)
import CommonState (..)

developerTab : (Int, Int) -> Element
developerTab (w,h) =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH)
    in container w h middle screen'

screen : (Int, Int) -> Element
screen (w,h) =
    container w h midTop
        <| flow down
            [ widget (w, 64) "Media:"
            , spacer 1 16
            , widget (w, 64) "Flash:"
            , spacer 1 16
            , widget (w, 64) "Eeprom:"
            ]

widget : (Int, Int) -> String -> Element
widget (w,h) str =
    let (w',h') = (toFloat w, toFloat h)
        background = collage w h
               [filled grey
                   <| roundedRect w' h'
                   <| (max w' h') / 80
               ]
        txt = flow right [spacer 16 1, leftAligned <| text str]
    in layers [ background
              , container w h midLeft txt
              , container w h midRight buttons
              ]

buttons = flow right [ button (send commonActions (CommonNoOp)) "import"
                     , spacer 10 1
                     , button (send commonActions (CommonNoOp)) "export"
                     , spacer 16 1
                     ]
