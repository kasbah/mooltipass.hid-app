module ManageTab where

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


manageTab : (Int, Int) -> MemoryInfo -> Element
manageTab (w,h) i =
    let contentH = h - 32
        contentW = w - 64
        content' = container w contentH middle
            <| content (contentW, contentH) i
    in container w h middle content'

content : (Int, Int) -> MemoryInfo -> Element
content (w,h) info =
    let saveButton = button (send commonActions CommonNoOp) "save"
        cancelButton = button (send commonActions CommonNoOp) "cancel"
    in container w h midTop <| flow down
        [ favorites w info
        , spacer 1 heights.manageSpacer
        , credentials
            ( w
            , h - (heightOf (favorites w info))
                - heights.manageSpacer
                - (heights.button + 4)
                - heights.manageSpacer)
            info
        , spacer 1 heights.manageSpacer
        , container w (heights.button + 4) middle
            <| flow right [cancelButton, spacer 16 1, saveButton]
        ]

favorites : Int -> MemoryInfo -> Element
favorites w info =
    let style =
            Html.Attributes.style
                [ ("overflow-y", "auto")
                , ("width", toString (w - 16) ++ "px")
                , ("height", toString ch ++ "px")
                ]
        favorites' = Html.div [style]
                    (intersperse (Html.fromElement (spacer 1 5))
                        (map (favorite (w - 48)) (map2 (,) [1..15] (stripNothing info.favorites)))
                    )
                |> Html.toElement (w - 32) ch
        ch = heights.manageLogin * 5 + (5*6)
    in box (w, ch + 20 + heights.manageTitle) "Favorites"
        <| flow down
            [ spacer 1 10
            , flow right [spacer 16 1, favorites']
            ]

favorite : Int -> (Int, Maybe (String, String)) -> Html
favorite w (n,maybeF) =
    let service = layers [sBg lightGrey', sTxt]
        fh      = heights.manageLogin
        fh'     = toFloat fh
        sw      = (2 * w)//5 - (2 * spw) - iw
        sw'     = toFloat sw
        sBg c   = collage sw fh
            [roundedRectShape Left sw' fh' 5 |> filled c]
        sTxt    = container sw fh midLeft sTxt'
        sTxt' = flow right
            [spacer 5 1, leftAligned <| whiteText serviceString]
        login   = layers [lBg lightGrey', lTxt]
        lBg c   = collage lw fh [rect lw' fh' |> filled c]
        iw = 32
        iw' = toFloat iw
        lw       = ((3 * w)//5) - (2 * spw) - (2 * iw)
        lw'      = toFloat lw
        lTxt'    = flow right
            [spacer 5 1, leftAligned <| whiteText loginString]
        lTxt     = container lw fh midLeft lTxt'
        sp       = spacer spw 1
        spw      = 2
        upIcon     = Input.customButton
            (send guiActions NoOp) iUpUp iUpHover iUpDown
        iUpUp      = layers [iUpBg lightGrey' , upIcon']
        iUpHover   = layers [iUpBg lightGrey'', upIcon']
        iUpDown    = layers [iUpBg lightGrey'', upIcon']
        upIcon' = container iw fh middle
            <| image 18 18 ("images/arrow-up.svg")
        iUpBg  c  = collage iw fh
            [rect iw' fh' |> filled c]
        downIcon     = Input.customButton
            (send guiActions NoOp) iDownUp iDownHover iDownDown
        iDownUp      = layers [iDownBg lightGrey' , downIcon']
        iDownHover   = layers [iDownBg lightGrey'', downIcon']
        iDownDown    = layers [iDownBg lightGrey'', downIcon']
        downIcon' = container iw fh middle
            <| image 18 18 ("images/arrow-down.svg")
        iDownBg  c  = collage iw fh
            [rect iw' fh' |> filled c]
        rect' w h c = collage w h [rect (toFloat w) (toFloat h) |> filled c]
        (serviceString, loginString) = Maybe.withDefault ("","") maybeF
        icon f b = if b then f else rect' iw fh lightGrey'
        elem = if maybeF == Nothing then spacer w fh
               else flow right
                   [ service
                   , sp, login
                   , sp, icon upIcon (n /= 15)
                   , sp, icon downIcon (n /= 1)
                   , sp, favIcon True (serviceString,loginString)
                   ]
    in Html.div
       [Html.Attributes.style [("position", "relative")]]
       [Html.fromElement elem]

credentials : (Int, Int) -> MemoryInfo -> Element
credentials (w,h) i =
    let ht = heights.manageTitle
        titleBg = collage w ht
           [filled lightGrey <| roundedRectShape Top (toFloat w) (toFloat ht) 5]
        txt s = container w heights.manageTitle middle
            <| leftAligned (whiteText s)
        bg = roundedRect w h darkGrey'
        ch = h - ht - 20
        style =
            Html.Attributes.style
                [ ("overflow-y", "auto")
                , ("width", toString (w - 16) ++ "px")
                , ("height", toString ch ++ "px")
                ]
        credentials' = Html.div [style]
                    (intersperse (Html.fromElement (spacer 1 5))
                        (map (service (w - 48) i.favorites ) i.credentials)
                    )
                |> Html.toElement (w - 32) ch
    in box (w,h) "All Credentials"
        <| flow down
            [ spacer 1 10
            , flow right [spacer 16 1, credentials']
            ]

service : Int -> List (Maybe (String,String)) -> (String, List String) -> Html
service w favs (serviceString, loginStrings) =
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
                    (map
                        (\l -> login (w - 64) (serviceString,l) ((serviceString,l) `member` (justs favs)))
                        loginStrings
                    )
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

login : Int -> (String,String) -> Bool -> Element
login w (serviceString,loginString) fav =
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
            [spacer 5 5 , leftAligned <| whiteText loginString]
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
            [spacer 5 1, leftAligned <| whiteText "********"]
        ptxt     = container pw lh midLeft ptxt'
        lh       = heights.manageLogin
        lh'      = toFloat lh
        delIcon     = Input.customButton
            (send guiActions NoOp) iDelUp iDelHover iDelDown
        iDelUp      = layers [iDelBg lightGrey' , delIcon']
        iDelHover   = layers [iDelBg lightGrey'', delIcon']
        iDelDown    = layers [iDelBg lightGrey'', delIcon']
        delIcon' = container iw lh middle
            <| image 18 18 ("images/cross.svg")
        iDelBg  c  = collage iw lh
            [rect iw' lh' |> filled c]
        sp       = spacer spw 1
        spw      = 2
        iw       = 32
        iw'      = toFloat iw
    in flow right [username, sp, password, sp, delIcon, sp, favIcon fav (serviceString,loginString)]


favIcon isFav credential =
    let iFavC fav     = if fav then favIcon' "blue" else favIcon' "white"
        iFavUp fav    = layers [iFavBg lightGrey' , iFavC fav]
        iFavHover fav = layers [iFavBg lightGrey'', iFavC fav]
        iFavDown      = layers [iFavBg lightGrey'', favIcon' "blue" ]
        iFavBg  c     = collage iw lh
            [roundedRectShape Right iw' lh' 5 |> filled c]
        favIcon' c    = container iw lh middle
            <| image 18 18 ("images/lightning-" ++ c ++ ".svg")
        lh            = heights.manageLogin
        lh'           = toFloat lh
        iw       = 32
        iw'      = toFloat iw
    in Input.customButton
        (send commonActions
            (if isFav then RemoveFromFavs credential else AddToFavs credential))
        (iFavUp isFav)
        (iFavHover isFav)
        iFavDown

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
