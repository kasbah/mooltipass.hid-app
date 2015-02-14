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
    let contentH = h - 32
        contentW = w - 64
        content' = container w contentH middle
            <| content (contentW, contentH) i
    in container w h middle content'

content : (Int, Int) -> MemoryInfo -> Element
content (w,h) info =
    let favHeight =
            (min 5 (length info.favorites)) * 48
            + heights.manageTitle
        saveButton = container w
            (heights.button + 4)
            middle
            <| button (send commonActions CommonNoOp) "save"
    in container w h midTop <| flow down
        [ favorites (w,favHeight) info
        , spacer 1 heights.manageSpacer
        , credentials
            ( w
            , h - favHeight
                - heights.manageSpacer
                - (heights.button + 4)
                - heights.manageSpacer)
            info
        , spacer 1 heights.manageSpacer
        , saveButton
        ]

favorites : (Int, Int) -> MemoryInfo -> Element
favorites (w,h) info = box (w,h) "Favorites" Element.empty

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let ht = heights.manageTitle
        titleBg = collage w ht
           [filled lightGrey <| roundedRectShape Top (toFloat w) (toFloat ht) 5]
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
        credentials' = Html.div [style]
                    (intersperse (Html.fromElement (spacer 1 5))
                        (map (service (w - 48)) i.credentials)
                    )
                |> Html.toElement (w - 32) (h - 32)
    in box (w,h) "Credentials" <| flow down [spacer 1 10, flow right [spacer 16 1, credentials']]

service : Int -> (String, List String) -> Html.Html
service w (serviceString, loginStrings) =
    let bg = roundedRect w h lightGrey
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
        h = ch + (length loginStrings) * (32 + 5) + 5
    in Html.div
       [Html.Attributes.style [("position", "relative")]]
       [Html.fromElement
            <| layers
                [ bg
                , container w h topLeft
                    <| flow down [title, spacer 1 5, logins]
                ]
       ]

login : Int -> String -> Element
login w loginString =
    let username = layers [ubg, utxt]
        uw       = (w//2) - spw
        uw'      = toFloat uw
        ubg      = collage uw lh
            [roundedRectShape Left uw' lh' 5 |> filled lightGrey']
        utxt'    = flow right
            [spacer 5 5 , leftAligned <| whiteText loginString]
        utxt     = container uw lh midLeft utxt'
        password = layers [pbg, ptxt]
        pw       = (w//2) - spw - iw
        pw'      = toFloat pw
        pbg      = collage pw lh
            [rect pw' lh' |> filled lightGrey']
        ptxt'    = flow right
            [spacer 5 1, leftAligned <| whiteText "********"]
        ptxt     = container pw lh midLeft ptxt'
        lh       = heights.manageLogin
        lh'      = toFloat lh
        icon     = layers [ibg,icon']
        iw       = 32
        iw'      = toFloat iw
        ibg      = collage iw lh
            [roundedRectShape Right iw' lh' 5 |> filled lightGrey']
        icon'    = container iw lh middle
            <| image 18 18 "images/lightning.svg"
        sp       = spacer spw 1
        spw      = 2
    in flow right [username, sp, password, sp, icon]

title : Int -> String -> Element
title w str =
    let ht    = heights.manageTitle
        bg    = collage w ht
           [filled lightGrey <| roundedRectShape Top (toFloat w) (toFloat ht) 5]
        txt   = container w heights.manageTitle middle
            <| leftAligned (whiteText str)
    in layers [bg, txt]

box : (Int, Int) -> String -> Element -> Element
box (w,h) str cont =
    let bg = roundedRect w h grey
    in layers [bg, flow down [title w str, cont]]
