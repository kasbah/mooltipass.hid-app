module DeveloperTab where

import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (..)
import Signal
import Text (..)
import String

import Color
import Layout (..)
import CustomGraphics (..)
import Actions (..)
import CommonState (..)
import GuiState (..)

developerTab : (Int, Int) -> TransferInfo -> Element
developerTab (w,h) t =
    let screenH = h - 32
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) t
    in container w h middle screen'

screen : (Int, Int) -> TransferInfo -> Element
screen (w,h) t =
    container w h midTop
        <| flow down
            [ widget (w, 64) t
            , container w 64 middle
                <| bigButton
                    (send guiActions (SetImportMedia Requested))
                    "import media"
            ]

infoText : TransferInfo -> String
infoText t = case t of
    ImportRequested id -> "importing " ++ fileName id
    Importing id _ _   -> "importing " ++ fileName id
    Imported id        -> "successfully imported " ++ fileName id
    TransferError str  -> "import error: " ++ str
    _                  -> ""

widget : (Int, Int) -> TransferInfo -> Element
widget (w,h) t =
    let (w',h') = (toFloat w, toFloat h)
        pcToWidth x = w' * (toFloat x)/100
        bg = collage w h
           [ filled grey
               <| roundedRect w' h'
               <| 5
           ]
        progress pc c = collage (round (pcToWidth pc)) h
           [ alpha 0.5 <| filled c
               <| roundedRect (pcToWidth pc) h'
               <| 5
           ]
        txt s = container w h midLeft
            <| flow right [spacer 16 1, leftAligned (text s)]
    in case t of
        _ -> layers [bg,progress 25 cyan,txt (infoText t) ]
  --      ImportRequested p ->
  --      Importing p i
  --      Imported p
  --      TransferError str
  --      NoTransfer

  --      background = collage w h
  --      foreground = collage w h
  --             [
  --             ]
  --
  --  in layers [ background
  --            , container w h midLeft txt
  --            ]
