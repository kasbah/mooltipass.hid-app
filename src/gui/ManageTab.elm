module ManageTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color
import Text (..)
import Text

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
        txt s =
            container w heights.manageTitle middle
                <| leftAligned (whiteText s)
        title = layers [title', txt "Favorites"]
        bg =  collage w h
           [ filled grey
               <| roundedRect w' h' 5
           ]
    in layers [bg, title]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
        titleBg = collage w heights.manageTitle
           [dome lightGrey w' heights.manageTitle 5]
        txt s = container w heights.manageTitle middle
            <| leftAligned (whiteText s)
        title = layers [titleBg, txt "Credentials"]
        bg =  collage w h
           [filled grey <| roundedRect w' h' 5]
        content = container w (h - heights.manageTitle) midTop
            <| flow down
            <| intersperse (spacer 1 5)
            <| map (credential (w - 16)) i.credentials
    in layers [bg, flow down [title,content]]

credential : Int -> (String, List String) -> Element
credential w (contextString, loginStrings) =
    let bg = collage w l
           [ filled darkGrey'
               <| roundedRect (toFloat w) (toFloat l) 5
           ]
        context' = leftAligned <| Text.height 14 <| whiteText contextString
        cw = widthOf context' + 32
        ch = heightOf context' + 10
        (cw',ch') = (toFloat cw, toFloat ch)
        contextBg = collage cw ch [roundedRect cw' ch' (ch'/4) |> filled lightGrey]
        context = layers [contextBg, container cw ch middle context']
        title  = flow right
                [ spacer 8 1
                , context
                ]
        logins = flow right [spacer 32 1, flow down (map login loginStrings)]
        login str = leftAligned <| whiteText str
        l = ch + (length loginStrings) * 32
    in layers [bg, container w l topLeft <| flow down [title, spacer 1 5, logins]]

