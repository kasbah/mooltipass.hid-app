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

-- extra libraries
import Html
import Html.Attributes

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
        bg =  roundedRect w h grey
    in layers [bg, title]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
        titleBg = collage w heights.manageTitle
           [dome lightGrey w' heights.manageTitle 5]
        txt s = container w heights.manageTitle middle
            <| leftAligned (whiteText s)
        title = layers [titleBg, txt "Credentials"]
        bg = roundedRect w h darkGrey'
        style =
            Html.Attributes.style
                [ ("overflow-y", "auto")
                , ("width", toString (w - 16) ++ "px")
                , ("height", toString (h - (heightOf title) - 10) ++ "px")
                ]
        content = Html.div [style]
                    (intersperse (Html.fromElement (spacer 1 5))
                        (map (credential maxLoginW (w - 48)) i.credentials)
                    )
                |> Html.toElement (w - 32) (h - 32)
        maxLoginW' ls = foldr (\str z -> max (widthOf (loginElem str)) z) 0 ls
        maxLoginW = foldr (\(_,ls) z -> max (maxLoginW' ls) z) 0 i.credentials
    in layers [bg, flow down [title, flow right [spacer 16 1, content]]]

credential : Int -> Int -> (String, List String) -> Html.Html
credential maxLoginW w (contextString, loginStrings) =
    let bg = roundedRect w l lightGrey
        context' = leftAligned <| Text.height 14 <| whiteText contextString
        cw = widthOf context' + 32
        ch = heightOf context' + 10
        (cw',ch') = (toFloat cw, toFloat ch)
        contextBg = roundedRect cw ch lightGrey'
        context   = layers [contextBg, container cw ch middle context']
        title  = flow right [ spacer 8 1, context ]
        logins = flow right [spacer 32 1, flow down (intersperse (spacer 5 5 ) (map (login maxLoginW w) loginStrings))]
        l = ch + (length loginStrings) * (32 + 5) + 5
    in Html.div [Html.Attributes.style [("position", "relative")]] [Html.fromElement <| layers [bg, container w l topLeft <| flow down [title, spacer 1 5, logins]]]

login : Int -> Int -> String -> Element
login maxL w loginString =
    let login'' = loginElem loginString
        login' = container (widthOf login'') lh midLeft login''
        llw = widthOf login' + 16
        llh = heightOf login' + 4
        pad = (maxL + 16) - llw + 5
        lw = w - 64
        lh = 32
        lightning = image 18 18 "images/lightning.svg"
        icons = flow left [container lh lh middle lightning, bar]
        password' = leftAligned <| whiteText "********"
        password = container (widthOf password') lh midLeft password'
        bar = collage 2 lh [rect 2 lh |> filled lightGrey]
        bg = roundedRect lw lh lightGrey'
        txts = flow right [spacer 5 1, login', spacer pad 1, bar, spacer 5 1, password]
        sp = spacer (lw - (widthOf txts) - (widthOf icons)) 1
    in layers [bg, flow right [txts, sp, icons]]

loginElem str =  flow right [spacer 5 5 , leftAligned <| whiteText str]
