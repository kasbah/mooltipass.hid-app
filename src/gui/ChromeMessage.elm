module ChromeMessage where

-- local source
import GuiState (..)

type alias ToChromeMessage =  {pickMediaFile : Maybe ()}

emptyToChromeMessage = { pickMediaFile = Nothing }

encode : GuiState -> (ToChromeMessage, Action)
encode s =
    let e = emptyToChromeMessage
    in case s.importMedia of
        Requested -> ({e | pickMediaFile <- Just ()}, SetImportMedia Waiting)
        _         -> ({e | pickMediaFile <- Nothing}, NoOp)
