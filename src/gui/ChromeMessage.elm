module ChromeMessage where

-- local source
import GuiState (..)
import CommonState  (..)

type alias ToChromeMessage =
    { pickMediaFile : Maybe ()
    , writeMemFile  : Maybe MemInfoData
    , readMemfile   : Maybe ()
    }

emptyToChromeMessage =
    { pickMediaFile = Nothing
    , writeMemFile  = Nothing
    , readMemfile   = Nothing
    }

encode : GuiState -> (ToChromeMessage, Action)
encode s =
    let e = emptyToChromeMessage
    in if | s.importMedia == Requested ->
        ({e | pickMediaFile <- Just ()}, SetImportMedia Waiting)
       | s.writeMem == Requested -> case s.unsavedMemInfo of
            MemInfo d -> ({e | writeMemFile <- Just d}, SetWriteMem Waiting)
            _ -> (e,NoOp)
       | otherwise -> (e,NoOp)

type alias FromChromeMessage =
    { pickedMediaFile : Maybe String
    , writeMemFile    : Maybe ()
    , readMemFile     : Maybe MemInfoData
    }

decode : FromChromeMessage -> Action
decode m = case m.pickedMediaFile of
    Just p -> SetImportMedia (RequestFile p)
    Nothing -> NoOp
