module DeveloperTab where

-- Elm standard library
import Graphics.Element as Element
import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Signal
import Text (..)
import String

-- local source
import Color
import Layout (..)
import CustomGraphics (..)
import Actions (..)
import CommonState (..)
import GuiState (..)

developerTab : (Int, Int) -> ImportInfo -> Element
developerTab (w,h) t =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) t
    in container w h middle screen'

screen : (Int, Int) -> ImportInfo -> Element
screen (w,h) t =
    container w h midTop
        <| flow down
            [ widget (w, 64) t
            , container w 64 middle
                <| bigButton
                    (send guiActions (SetImportMedia Requested))
                    "import media"
            ]

infoText : ImportInfo -> Text
infoText t = case t of
    ImportRequested id -> whiteText <| "importing " ++ fileName id
    Importing id _ _   -> whiteText <| "importing " ++ fileName id
    Imported id        -> text <| "sucessfully imported " ++ fileName id
    ImportError str    -> text <| "import error: " ++ str
    _                  -> text <| ""

widget : (Int, Int) -> ImportInfo -> Element
widget (w,h) t =
    let (w',h') = (toFloat w, toFloat h)
        progToWidth x = w' * x
        bg = roundedRect w h grey
        progressBar' prog c = roundedRect (round (progToWidth prog)) h c
        progressBar = case t of
            ImportRequested id -> Element.empty
            Importing id td ttl ->
                progressBar' (toFloat (ttl - td)/toFloat ttl) blue
            Imported id        -> progressBar' 1.0 cyan
            ImportError str  -> progressBar' 1.0 Color.red
            _                  -> Element.empty
        txt s = container w h middle <| leftAligned s
    in layers [bg, progressBar, txt (infoText t) ]
