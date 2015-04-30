module FromGuiMessage where

-- Elm standard library
import Maybe

-- local source
import Byte (..)
import CommonState (..)

type alias FromGuiMessage =
    { setLog           : Maybe (List String)
    , getState         : Maybe ()
    , startImportMedia : Maybe FileId
    , startMemManage   : Maybe ()
    , endMemManage     : Maybe ()
    , saveMemManage    : Maybe MemInfoData
    , setParameter     : Maybe (Maybe (Int, Byte))
    , getParameter     : Maybe (Maybe Int)
    }

emptyFromGuiMessage =
    { setLog           = Nothing
    , getState         = Nothing
    , startImportMedia = Nothing
    , startMemManage   = Nothing
    , endMemManage     = Nothing
    , saveMemManage    = Nothing
    , setParameter     = Nothing
    , getParameter     = Nothing
    }

encode : CommonAction -> FromGuiMessage
encode action =
    let e = emptyFromGuiMessage
    in case action of
        SetLog l           -> {e | setLog <- Just l}
        StartImportMedia p -> {e | startImportMedia <- Just p}
        StartMemManage     -> {e | startMemManage <- Just ()}
        EndMemManage       -> {e | endMemManage <- Just ()}
        SaveMemManage d    -> {e | saveMemManage <- Just d}
        SetParameter mpb   -> {e | setParameter <- Just
                                (Maybe.map (\(p,b) -> (encodeParameter p, b)) mpb)} 
        GetParameter mp    -> {e | getParameter <- Just (Maybe.map encodeParameter mp)} 
        _                  -> e

decode :  FromGuiMessage -> CommonAction
decode msg =
    let decode' =
        Maybe.oneOf
            [ Maybe.map SetLog msg.setLog
            , Maybe.map StartImportMedia msg.startImportMedia
            , Maybe.map (\_ -> GetState) msg.getState
            , Maybe.map (\_ -> StartMemManage) msg.startMemManage
            , Maybe.map (\_ -> EndMemManage) msg.endMemManage
            , Maybe.map SaveMemManage msg.saveMemManage
            , Maybe.map (\x -> SetParameter (Maybe.map (\(p,b) -> (decodeParameter p, b)) x)) msg.setParameter
            , Maybe.map (\x -> GetParameter (Maybe.map decodeParameter x)) msg.getParameter
            ]
    in Maybe.withDefault CommonNoOp decode'
