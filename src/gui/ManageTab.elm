module ManageTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color
import Text (..)

-- local source
import CommonState (..)
import CustomGraphics (..)
import Layout (..)

manageTab : (Int, Int) -> MemoryInfo -> Element
manageTab (w,h) i =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) i
    in container w h middle screen'

screen : (Int, Int) -> MemoryInfo -> Element
screen (w,h) i =
    let favHeight = (min 5 (length i.favorites)) * 48 + heights.manageTitle
    in container w h midTop <| flow down
        [ favorites (w,favHeight) i
        , spacer 1 heights.manageSpacer
        , credentials (w, h - favHeight - heights.manageSpacer) i
        ]

favorites : (Int, Int) -> MemoryInfo -> Element
favorites (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
        title' = collage w heights.manageTitle
           [ dome lightGrey w' heights.manageTitle 5
           ]
        txt s = container w heights.manageTitle middle <| leftAligned (text s)
        title = layers [title', txt "Favorites"]
        bg =  collage w h
           [ filled grey
               <| roundedRect w' h' 5
           ]
    in layers [bg, title]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
        title' = collage w heights.manageTitle
           [ dome lightGrey w' heights.manageTitle 5
           ]
        txt s = container w heights.manageTitle middle <| leftAligned (text s)
        title = layers [title', txt "Credentials"]
        bg =  collage w h
           [ filled grey
               <| roundedRect w' h' 5
           ]
    in layers [bg, title]
