module ChromeBgMessage where

-- Elm standard library
import List
-- local source
import BackgroundState (..)
import CommonState (..)
import Byte (..)
import DevicePacket (..)

type alias ToChromeMessage = {readFile : Maybe String}

emptyToChromeMessage = {readFile = Nothing}

encode : BackgroundState -> ToChromeMessage
encode s =
    let e = emptyToChromeMessage
    in case s.mediaImport of
        MediaImportRequested p -> {e | readFile <- Just p}
        _ -> e

type alias FromChromeMessage = {readFile : List ByteArray}

decode : FromChromeMessage -> BackgroundAction
decode msg = case msg.readFile of
    [] -> NoOp
    is -> SetMediaImport (MediaImportStart (List.map AppImportMedia is))
