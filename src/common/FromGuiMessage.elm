module FromGuiMessage where

-- Elm standard library
import Maybe

-- local source
import CommonState (..)

type alias FromGuiMessage =
    { setLog           : Maybe (List String)
    , getState         : Maybe ()
    , startImportMedia : Maybe FileId
    , startMemManage   : Maybe ()
    }

emptyFromGuiMessage =
    { setLog           = Nothing
    , getState         = Nothing
    , startImportMedia = Nothing
    , startMemManage   = Nothing
    }

encode : CommonAction -> FromGuiMessage
encode action =
    let e = emptyFromGuiMessage
    in case action of
        SetLog l           -> {e | setLog <- Just l}
        StartImportMedia p -> {e | startImportMedia <- Just p}
        StartMemManage     -> {e | startMemManage <- Just ()}
        _                  -> e

decode :  FromGuiMessage -> CommonAction
decode msg =
    let decode' =
        Maybe.oneOf
            [ Maybe.map SetLog msg.setLog
            , Maybe.map StartImportMedia msg.startImportMedia
            , Maybe.map (\_ -> GetState) msg.getState
            , Maybe.map (\_ -> StartMemManage) msg.startMemManage
            ]
    in Maybe.withDefault CommonNoOp decode'
