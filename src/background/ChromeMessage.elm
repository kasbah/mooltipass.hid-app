module ChromeMessage where

-- local source
import BackgroundState (..)

type alias ToChromeMessage = {readFile : Maybe String}

emptyToChromeMessage = {readFile = Nothing}

encode : BackgroundState -> ToChromeMessage
encode s =
    let e = emptyToChromeMessage
    in case s.mediaImport of
        MediaImportRequested p -> {e | readFile <- Just p}
        _ -> e


