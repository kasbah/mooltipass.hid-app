module SettingsTab where

-- Elm standard library
import Dict
import Dict (Dict)
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Graphics.Input.Field (defaultStyle, noContent, Content, Selection)
import List (..)
import Color
import String (toInt)
import Text (..)
import Text
-- import Signal (channel, send, subscribe, Message)
import Signal (..)
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
import KeyboardLayout (..)

import Debug

settingsTab : (Int, Int) -> SettingsInfo -> Dict Int Selection -> Element
settingsTab (w,h) settings selections =
    let contentH = h - 32
        contentW = w - 64
        content' = container w contentH middle
            <| content (contentW, contentH) settings selections
    in container w h middle content'

content : (Int, Int) -> SettingsInfo -> Dict Int Selection -> Element
content (w,h) settings selections =
    let content' = container w h midTop <| flow down
            [ -- cardSettings (w,120) ,
              mpSettings (w,120) settings selections
            ]
    in content'

{-
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
-}

mpSettings : (Int, Int) -> SettingsInfo -> Dict Int Selection -> Element
mpSettings (w,h) settings selections =
    let
        noSelection = Selection 0 0 Field.Forward
        timeoutSelection = Maybe.withDefault noSelection <|
                             Dict.get (encodeParameter UserInterTimeout) selections
        timeoutContent = Maybe.map (\x -> Content (toString x) timeoutSelection) settings.timeout
        mpSettings' = container w h midTop <| flow down
            --[ sel (w - 32) "Keyboard layout" setKeyboard (sortBy fst allKeyboards)
            [ field (w - 32) "User interaction timeout" (sendIntContent UserInterTimeout) timeoutContent
            , labelCheckbox (w - 32) "Offline Mode" (sendBool OfflineMode) (settings.offline)
            , labelCheckbox (w - 32) "Screensaver" (sendBool ScreenSaver) (settings.screensaver)
            , labelCheckbox (w - 32) "Flash Screen" (sendBool FlashScreen) (settings.flashscreen)
            ]
    in box (w,h) "Mooltipass Settings"
        <| flow down
            [ spacer 1 10
            , flow right [spacer 16 1, mpSettings']
            ]

{-

Commands
========
https://github.com/limpkin/mooltipass/tree/master/source_code/src/USB

0xB1: Set Mooltipass parameter
  From plugin/app: Set Mooltipass parameter, first byte is the param ID, second is the value
  From Mooltipass: 1 byte data packet, 0x00 indicates that the request wasn't performed, 0x01 if so
0xB2: Get Mooltipass parameter
  From plugin/app: Get parameter from Mooltipass, first byte is the param ID
  From Mooltipass: The param value

<Nistur> Hmmm. The 64 byte packages, the first 2 bytes are defined, assuming the rest is a string, for
example with the context, login or password, are they null terminated?
<limpkin> they are
<limpkin> and the length includes that null termination
<limpkin> so length = strlen(string) + 1

ParamIDs
========
https://github.com/limpkin/mooltipass/blob/master/source_code/src/LOGIC/logic_eeprom.h#L44

#define KEYBOARD_LAYOUT_PARAM               1
#define USER_INTER_TIMEOUT_PARAM            2

Keyboards
=========

https://github.com/limpkin/mooltipass/tree/master/bitmaps

<limpkin> from 18 to 39
<limpkin> parameter obviously is 0 to 20
<limpkin> for example en us is 128 + 18
<limpkin> fyi, tested on the python script
<limpkin> switchting to french was 0x02 0xB1 0x01 0x93

Elm
===
https://github.com/kasbah/mooltipass.hid-app/blob/master/src/background/DevicePacket.elm#L217

Really want to:
send toDevice (OutgoingSetParameter KeyboardLayout kb)
send toDevice (OutgoingSetParameter UserInterTimeout t)

<kfish> kasbah: if i have cmd=(OutgoingSetParameter KeyboardLayout 0x93), and I want to "send
toDevice cmd", what's the procedure? ie. handling the response etc.
<kfish> i'm looking at DeviceMessage.sendMessage', is that a good place to start?
<kasbah> kfish: DeviceMessage.encode
<kasbah> encodes the background state into a message to send to the device
<kasbah> (and also any actions that change the state due to having sent a message)
<kasbah> but from the GUI you need to go
GUI->JS->chrome.runtime.message->JS->Background->JS->chrome.runtime.hid
<kfish> ok, so i (send guiAction ...) passing it some new gui action?
<kasbah> if it needs to go to the bg then you need to (send commonActions ...)
<kasbah> which takes a CommonAction from common/CommonState.elm
<kasbah> common as in common to background and gui
<kfish> ok
<kasbah> basically the GUI state is lost when the GUI closes
<kasbah> but the background keeps running
<kfish> ok
<kfish> and what's the common action for sending a devicemessage?
<kasbah> unfortunately it's not that direct
<kfish> extNeedsToSend?
<kasbah> so the way i have been doing this:
<kasbah> add a CommonAction for what you want to do
<kasbah> write the message encoding and decoding for that
<kasbah> then write the state updates according to that action
<kasbah> probably adding something to the states to hold that info
<kasbah> then turn that state info into a message for the device
<limpkin> that looks quite complex to send a message...
<kasbah> yes
<kasbah> if you can think of a way to cut through the fat...

...

<kasbah> kfish: this is what you'll have to modify to send messages to the device direct
<kasbah>
https://github.com/kasbah/mooltipass.hid-app/blob/39d80a0d593b93bcb7fe74ffd54197de127f2260/src/gui/Main.elm#L48
<kasbah> to receive messages there is some routing in background/device.js
<kasbah> and a TODO in gui/load-elm.js to add an Elm port

-}

labelCheckbox : Int -> String -> (Bool -> Message) -> Maybe Bool -> Element
labelCheckbox w kString act valm =
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
        password = case valm of
                       Just val -> Input.checkbox act val
                       Nothing  -> flow right [spacer 5 5, leftAligned <| whiteText "Loading ..."]
        pw       = (w//2) - (2*spw) - (2*iw)
        pw'      = toFloat pw
        pbg c    = collage pw lh [rect pw' lh' |> filled c]
        lh       = heights.settingsLogin
        lh'      = toFloat lh
        sp       = spacer spw 1
        spw      = 2
        iw       = 32
        iw'      = toFloat iw
    in flow right [username
                  , sp, password
                  ]


field : Int -> String -> (Content -> Message) -> Maybe Content -> Element
field w kString act vStringm =
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
        password = case vStringm of
                       Just vString -> Field.field Field.defaultStyle act kString vString
                       Nothing  -> flow right [spacer 5 5, leftAligned <| whiteText "Loading ..."]
        lh       = heights.settingsLogin
        lh'      = toFloat lh
        sp       = spacer spw 1
        spw      = 2
        iw       = 32
        iw'      = toFloat iw
    in flow right [username
                  , sp, password
                  ]

{-
 A dropDown is a native <select> with <option>s:

     var drop = NativeElement.createNode('select');
        drop.style.border = '0 solid';
        drop.style.pointerEvents = 'auto';
        drop.style.display = 'block';

     var option = NativeElement.createNode('option');

-}

textButton : Int -> String -> Message -> Element
textButton w l act =
    let bUp      = layers [ubg lightGrey', btxt]
        bHover   = layers [ubg lightGrey'', btxt]
        bDown    = bUp
        bw       = (w//2) - spw
        bw'      = toFloat bw
        ubg c    = collage bw lh [roundedRectShape Left bw' lh' 5 |> filled c]
        btxt'    = flow right [spacer 5 5 , leftAligned <| whiteText l]
        btxt     = container bw lh midLeft btxt'
        lh       = heights.settingsLogin
        lh'      = toFloat lh
        spw      = 2
        iw       = 32
        iw'      = toFloat iw
    in Input.customButton act bUp bHover bDown

sel : Int -> String -> (a -> Message) -> List (String, a) -> Element
sel w kString act things =
    let -- username = uUp -- button disabled for beta release
        -- username = Input.customButton (send guiActions NoOp) uUp uHover uDown
        -- username = Input.customButton (send commonActions (OutgoingSetParameter KeyboardLayout 0x93)) uUp uHover uDown
        -- username = Input.customButton (send guiActions (CommonAction (SetKeyboard 0x93))) uUp uHover uDown
        -- username = Input.customButton (send guiActions (CommonAction (GetParameter (Just KeyboardLayout)))) uUp uHover uDown
        username = Input.customButton (sendGetParameter UserInterTimeout) uUp uHover uDown
        uUp      = layers [ubg lightGrey', utxt]
        uHover   = layers [ubg lightGrey'', utxt]
        uDown    = uUp
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
            -- [spacer 5 1, Input.dropDown (\x -> send guiActions NoOp) things]
            -- [spacer 5 1, Input.dropDown (send kbChannel) things]
            [spacer 5 1, Input.dropDown act things]
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
