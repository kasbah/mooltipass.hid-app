module SettingsTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Input as Input
import List (..)
import Color
import Text (..)
import Text
import Signal (send)
import Maybe

-- extra libraries
import Html
import Html (Html)
import Html.Attributes

-- local source
import CommonState (..)
import GuiState (..)
import CustomGraphics (..)
import Layout (..)
import Actions (..)
import Util (..)
import Byte (..)

import Debug

settingsTab : (Int, Int) -> Element
settingsTab (w,h) =
    let contentH = h - 32
        contentW = w - 64
        content' = container w contentH middle
            <| content (contentW, contentH)
    in container w h middle content'

content : (Int, Int) -> Element
content (w,h) =
    let content' = container w h midTop <| flow down
            [ cardSettings (w,120)
            , mpSettings (w,120)
            ]
    in content'

cardSettings : (Int, Int) -> Element
cardSettings (w,h) =
    let cardAuth = field (w - 32) "Bob" "********"
        cardSettings' = container w h midTop <| flow down
            [ cardAuth
            ]
    in box (w,h) "Card Authentication"
        <| flow down
            [ spacer 1 10
            , flow right [spacer 16 1, cardSettings']
            ]

mpSettings : (Int, Int) -> Element
mpSettings (w,h) =
    let mpSettings' = container w h midTop <| flow down
            [ field (w - 32) "Keyboard layout" "azerty"
            , field (w - 32) "User interaction timeout" "77"
            ]
    in box (w,h) "Mooltipass Settings"
        <| flow down
            [ spacer 1 10
            , flow right [spacer 16 1, mpSettings']
            ]


field : Int -> String -> String -> Element
field w kString vString =
    let username = uUp -- button disabled for beta release
        --username = Input.customButton (send guiActions NoOp) uUp uHover uDown
        uUp      = layers [ubg lightGrey', utxt]
        --uHover   = layers [ubg lightGrey'', utxt]
        --uDown    = uUp
        uw       = (w//2) - spw
        uw'      = toFloat uw
        ubg c    = collage uw lh
            [roundedRectShape Left uw' lh' 5 |> filled c]
        utxt'    = flow right
            [spacer 5 5 , leftAligned <| whiteText kString]
        utxt     = container uw lh midLeft utxt'
        password = pUp -- button disabled for beta release
        --password = Input.customButton (send guiActions NoOp) pUp pHover pDown
        pUp      = layers [pbg lightGrey', ptxt]
        --pHover   = layers [pbg lightGrey'', ptxt]
        --pDown    = pUp
        pw       = (w//2) - (2*spw) - (2*iw)
        pw'      = toFloat pw
        pbg c    = collage pw lh [rect pw' lh' |> filled c]
        ptxt'    = flow right
            [spacer 5 1, leftAligned <| whiteText vString]
        ptxt     = container pw lh midLeft ptxt'
        lh       = heights.settingsLogin
        lh'      = toFloat lh
        sp       = spacer spw 1
        spw      = 2
        iw       = 32
        iw'      = toFloat iw
    in flow right [username
                  , sp, password
                  ]

title : Int -> String -> Element
title w str =
    let ht    = heights.settingsTitle
        bg    = collage w ht
           [filled lightGrey <| roundedRectShape Top (toFloat w) (toFloat ht) 5]
        txt   = container w heights.settingsTitle middle
            <| leftAligned (whiteText str)
    in layers [bg, txt]

box : (Int, Int) -> String -> Element -> Element
box (w,h) str cont =
    let bg = roundedRect w h grey
    in layers [bg, flow down [title w str, cont]]
