module ChromeMessage where

import Maybe

-- local source
import GuiState (..)
import CommonState (..)

type alias ToChromeMessage =
    { pickMediaFile : Maybe ()
    , writeMemFile  : Maybe MemInfoData
    , readMemFile   : Maybe ()
    }

emptyToChromeMessage =
    { pickMediaFile = Nothing
    , writeMemFile  = Nothing
    , readMemFile   = Nothing
    }

encode : GuiState -> (ToChromeMessage, Action)
encode s =
    let e = emptyToChromeMessage
    in if | s.importMedia == Requested ->
        ({e | pickMediaFile <- Just ()}, SetImportMedia Waiting)
       | s.writeMem -> case s.unsavedMemInfo of
            MemInfo d -> ({e | writeMemFile <- Just d}, SetWriteMem False)
            _ -> (e,NoOp)
       | s.readMem -> ({e | readMemFile <- Just ()}, SetReadMem False)
       | otherwise -> (e,NoOp)

type alias FromChromeMessage =
    { pickedMediaFile : Maybe String
    , readMemFile     : Maybe MemInfoData
    }

decode : FromChromeMessage -> Action
decode msg =
    let decode' =
        Maybe.oneOf
            [ Maybe.map (SetImportMedia << RequestFile) msg.pickedMediaFile
            , Maybe.map AddToUnsavedMem msg.readMemFile
            ]
    in Maybe.withDefault NoOp decode'
