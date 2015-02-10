module ManageTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)

-- local source
import CommonState (..)
import CustomGraphics (..)

manageTab : (Int, Int) -> MemoryInfo -> Element
manageTab (w,h) i =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) i
    in container w h middle screen'

screen : (Int, Int) -> MemoryInfo -> Element
screen (w,h) i =
    let favHeight = (min 5 (length i.favorites)) * 48
    in container w h midTop <| flow down
        [ favorites (w,favHeight) i
        , credentials (w, h - favHeight) i
        ]

favorites : (Int, Int) -> MemoryInfo -> Element
favorites (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
    in collage w h
           [ filled grey
               <| roundedRect w' h'
               <| 5
           ]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i = empty
