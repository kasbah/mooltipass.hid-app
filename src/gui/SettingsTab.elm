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
content (w,h) = Element.empty
