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
        ht = heights.manageTitle
        title' = collage w ht
           [filled lightGrey <| roundedRectShape Top w' (toFloat ht) 5]
        txt s =
            container w heights.manageTitle middle
                <| leftAligned (whiteText s)
        title = layers [title', txt "Favorites"]
        bg =  roundedRect w h grey
    in layers [bg, title]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let (w',h') = (toFloat w, toFloat h)
        ht = heights.manageTitle
        titleBg = collage w ht
           [filled lightGrey <| roundedRectShape Top w' (toFloat ht) 5]
        txt s = container w heights.manageTitle middle
            <| leftAligned (whiteText s)
        title = layers [titleBg, txt "Credentials"]
        bg = roundedRect w h darkGrey'
        style =
            Html.Attributes.style
                [ ("overflow-y", "auto")
                , ("width", toString (w - 16) ++ "px")
                , ("height", toString (h - (heightOf title) - 20) ++ "px")
                ]
        content = Html.div [style]
                    (intersperse (Html.fromElement (spacer 1 5))
                        (map (service (w - 48)) i.credentials)
                    )
                |> Html.toElement (w - 32) (h - 32)
    in layers [bg, flow down [title, spacer 1 10, flow right [spacer 16 1, content]]]

service : Int -> (String, List String) -> Html.Html
service w (serviceString, loginStrings) =
    let bg = roundedRect w l lightGrey
        service' = leftAligned <| Text.height 14 <| whiteText serviceString
        cw = widthOf service' + 32
        ch = heightOf service' + 10
        (cw',ch') = (toFloat cw, toFloat ch)
        serviceBg = roundedRect cw ch lightGrey'
        service   = layers [serviceBg, container cw ch middle service']
        title  = flow right [spacer 8 1, service]
        logins = flow right
            [ spacer 32 1
            , flow down
                (intersperse
                    (spacer 5 5)
                    (map (login (w - 64)) loginStrings)
                )
            ]
        l = ch + (length loginStrings) * (32 + 5) + 5
    in Html.div [Html.Attributes.style [("position", "relative")]] [Html.fromElement <| layers [bg, container w l topLeft <| flow down [title, spacer 1 5, logins]]]

login : Int -> String -> Element
login w loginString =
    let username = layers [ubg, utxt]
        uw       = (w//2) - spw
        uw'      = toFloat uw
        ubg      = collage uw lh
            [roundedRectShape Left uw' lh' 5 |> filled lightGrey']
        utxt'    = loginElem loginString
        utxt     = container uw lh midLeft utxt'
        password = layers [pbg, ptxt]
        pw       = (w//2) - spw - iw
        pw'      = toFloat pw
        pbg      = collage pw lh
            [rect pw' lh' |> filled lightGrey']
        ptxt'    = flow right [spacer 5 1, leftAligned <| whiteText "********"]
        ptxt     = container pw lh midLeft ptxt'
        lh       = heights.manageLogin
        lh'      = toFloat lh
        icon     = layers [ibg,icon']
        iw       = 32
        iw'      = toFloat iw
        ibg      = collage iw lh
            [roundedRectShape Right iw' lh' 5 |> filled lightGrey']
        icon'    = container iw lh middle <| image 18 18 "images/lightning.svg"
        sp       = spacer spw 1
        spw      = 2
    in flow right [username, sp, password, sp, icon]


loginElem str =  flow right [spacer 5 5 , leftAligned <| whiteText str]
