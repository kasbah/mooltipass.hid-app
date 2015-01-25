module ExtensionMessage where

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
    { connectState : Maybe String
    , credentials  : Maybe { context  : String
                           , login    : String
                           , password : String
                           }
    , noCredentials  : Maybe ()
    , updateComplete : Maybe ()
    }

emptyToExtensionMessage =
    { connectState   = Nothing
    , credentials    = Nothing
    , noCredentials  = Nothing
    , updateComplete = Nothing
    }

decode : FromExtensionMessage -> BackgroundAction
decode message =
    let decode' {ping, getInputs, update} =
        Maybe.oneOf
            [ Maybe.map (\_ -> SetExtAwaitingPing True) ping
            , Maybe.map (set ExtNeedsLogin) getInputs
            , Maybe.map (set ExtUpdate) update
            ]
        set constructor d = SetExtAwaitingData (constructor d)
        -- we do a bytestring conversion to check for errors but we just use
        -- the string above as ByteString is just a type alias
        errOrInputs = Maybe.map (\{context} -> byteString context) message.getInputs
        errOrUpdate = Maybe.map errOrUpdate' message.update
        errOrUpdate' {context, login, password} =
            let bSc = byteString context
                bSl = byteString login
                bSp = byteString password
            in bSc `andThen` (\_ -> bSl) `andThen` (\_ -> bSp) `andThen` (\_ -> Ok ())
    in case errOrUpdate of
        Just (Err err) -> CommonAction (AppendToLog ("Extension Error: " ++ err))
        _              -> case errOrInputs of
            Just (Err err) -> CommonAction (AppendToLog ("Extension Error: " ++ err))
            _              -> Maybe.withDefault NoOp (decode' message)

encode : BackgroundState -> (ToExtensionMessage, BackgroundAction)
encode s =
    let e = emptyToExtensionMessage
    in  if | s.extAwaitingPing ->
                ({ e | connectState <- Just <| case s.common.connected of
                               Connected    -> "connected"
                               NotConnected -> "disconnected"
                               _            -> "connected"
                }, SetExtAwaitingPing False)
           | s.extAwaitingData /= NoData -> case s.extAwaitingData of
                ExtCredentials    c  ->
                    ({e | credentials <- Just c}, SetExtAwaitingData NoData)
                ExtUpdateComplete _  ->
                    ({e | updateComplete <- Just ()}, SetExtAwaitingData NoData)
                ExtNoCredentials ->
                    ({e | noCredentials <- Just ()}, SetExtAwaitingData NoData)
                _ -> (e,NoOp)
           | otherwise -> (e, NoOp)
