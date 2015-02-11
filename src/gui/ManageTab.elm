module ManageTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import List (..)
import Color
import Text (..)
import Text
import Signal (send)

-- local source
import CommonState (..)
import CustomGraphics (..)
import Layout (..)
import Actions (..)


manageTab : (Int, Int) -> MemoryInfo -> Element
manageTab (w,h) i =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) i
    in container w h middle screen'

screen : (Int, Int) -> MemoryInfo -> Element
screen (w,h) i =
    let favHeight = (min 5 (length i.favorites)) * 48 + heights.manageTitle
        saveButton = container w (heights.logTabButton + 4) middle <| button (send commonActions CommonNoOp) "save"
    in container w h midTop <| flow down
        [ favorites (w,favHeight) i
        , spacer 1 heights.manageSpacer
        , credentials (w, h - favHeight - heights.manageSpacer - (heights.logTabButton + 4) - heights.manageSpacer) i

        , spacer 1 heights.manageSpacer
        , saveButton
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
    let bg = roundedRect' w l darkGrey
        context' = leftAligned <| Text.height 14 <| whiteText contextString
        cw = widthOf context' + 32
        ch = heightOf context' + 10
        (cw',ch') = (toFloat cw, toFloat ch)
        contextBg = collage cw ch [roundedRect cw' ch' (ch'/4) |> filled lightGrey]
        context   = layers [contextBg, container cw ch middle context']
        title  = flow right [ spacer 8 1, context ]
        logins = flow right [spacer 32 1, flow down (intersperse (spacer 5 5 ) (map (login w) loginStrings))]
        l = ch + (length loginStrings) * (32 + 5) + 5
    in layers [bg, container w l topLeft <| flow down [title, spacer 1 5, logins]]


roundedRect' w h c = collage w h [filled c <| roundedRect (toFloat w) (toFloat h) 5]

login : Int -> String -> Element
login w loginString =
    let login' = flow right [spacer 5 5 , leftAligned <| whiteText loginString]
        llw = widthOf login' + 16
        llh = heightOf login' + 4
        login'' = layers [roundedRect' llw  llh lightGrey, container llw llh middle login']
        lw = w - 64
        lh = 32
        lightning = image 18 18 "images/lightning.svg"
        icons = flow left [spacer 16 1, lightning]
        password' = leftAligned <| whiteText "********"
        pw = widthOf password' + 16
        ph = heightOf password' + 4
        password = layers [roundedRect' pw ph lightGrey, container pw ph middle password']
        bg = collage lw lh
            [ filled lightGrey' <| roundedRect (toFloat lw) (toFloat lh) 5]
    in layers [bg, container lw lh midLeft <| flow right [spacer 5 1, login'', spacer 5 1 , password], container lw lh midRight icons]

