module FromGuiMessage where

-- Elm standard library
import Maybe

-- local source
import CommonState (..)

type alias FromGuiMessage =
    { setLog           : Maybe (List String)
    , getState         : Maybe ()
    , startImportMedia : Maybe FileId
    , addToFavs        : Maybe (String, String)
    , removeFromFavs   : Maybe (String, String)
    }

emptyFromGuiMessage =
    { setLog           = Nothing
    , getState         = Nothing
    , startImportMedia = Nothing
    , addToFavs        = Nothing
    , removeFromFavs   = Nothing
    }

encode : CommonAction -> FromGuiMessage
encode action =
    let e = emptyFromGuiMessage
    in case action of
        SetLog l           -> {e | setLog <- Just l}
        StartImportMedia p -> {e | startImportMedia <- Just p}
        AddToFavs f        -> {e | addToFavs <- Just f}
        RemoveFromFavs f   -> {e | removeFromFavs <- Just f}
        _                  -> e

decode :  FromGuiMessage -> CommonAction
decode msg =
    let decode' =
        Maybe.oneOf
            [ Maybe.map SetLog msg.setLog
            , Maybe.map StartImportMedia msg.startImportMedia
            , Maybe.map AddToFavs msg.addToFavs
            , Maybe.map RemoveFromFavs msg.removeFromFavs
            ]
    in Maybe.withDefault CommonNoOp decode'
