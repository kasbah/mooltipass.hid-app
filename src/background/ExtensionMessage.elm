module ExtensionMessage where

-- Elm standard library
import Maybe

-- local source
import BackgroundState (..)
import CommonState as Common
import CommonState (..)
import Byte (..)
import Result (..)
import Util (..)

type alias FromExtensionMessage =
    { ping      : Maybe ()
    , getInputs : Maybe { context : String }
    , update    : Maybe { context  : String
                        , login    : String
                        , password : String
                        }
    }

type alias ToExtensionMessage =
    { deviceStatus : Maybe { connected : Bool, version : String }
    , credentials  : Maybe { context  : String
                           , login    : String
                           , password : String
                           }
    , noCredentials  : Maybe ()
    , updateComplete : Maybe ()
    }

emptyToExtensionMessage =
    { deviceStatus   = Nothing
    , credentials    = Nothing
    , noCredentials  = Nothing
    , updateComplete = Nothing
    }

decode : FromExtensionMessage -> BackgroundAction
decode message =
    let decode' {ping, getInputs, update} =
        Maybe.oneOf
            [ Maybe.map (\_ -> SetExtAwaitingPing True) ping
            , Maybe.map (set ExtWantsCredentials) getInputs
            , Maybe.map (set ExtWantsToWrite) update
            ]
        set constructor d = SetExtRequest (constructor d)
        -- we do a bytestring conversion to check for errors but we just use
        -- the string above as ByteString is just a type alias
        errOrInputs =
            Maybe.map
                (\{context} -> byteString context)
                message.getInputs
        errOrUpdate = Maybe.map errOrUpdate' message.update
        errOrUpdate' {context, login, password} =
            let bSc = byteString context
                bSl = byteString login
                bSp = byteString password
            in bSc `andThen` (\_ -> bSl)
                   `andThen` (\_ -> bSp)
                   `andThen` (\_ -> Ok ())
    in case errOrUpdate of
        Just (Err err) -> CommonAction
                            (AppendToLog ("Extension Error: " ++ err))
        _              -> case errOrInputs of
            Just (Err err) -> CommonAction
                                (AppendToLog ("Extension Error: " ++ err))
            _              -> Maybe.withDefault NoOp (decode' message)

encode : BackgroundState -> (ToExtensionMessage, BackgroundAction)
encode s =
    let e = emptyToExtensionMessage
    in  if | s.extAwaitingPing ->
            ({e | deviceStatus <- Just <|
                { connected = s.common.deviceStatus == Unlocked
                , version   = Maybe.withDefault "unknown"
                                (Maybe.map (\v -> v.version) s.deviceVersion)
                }
            }, SetExtAwaitingPing False)
           | s.extRequest /= NoRequest -> case s.extRequest of
                ExtCredentials    c  ->
                    ({e | credentials <- Just c}, SetExtRequest NoRequest)
                ExtWriteComplete _  ->
                    ({e | updateComplete <- Just ()}, SetExtRequest NoRequest)
                ExtNoCredentials ->
                    ({e | noCredentials <- Just ()}, SetExtRequest NoRequest)
                ExtNotWritten -> (e, SetExtRequest NoRequest)
                _ -> (e,NoOp)
           | otherwise -> (e, NoOp)
