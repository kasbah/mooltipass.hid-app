module ExtensionMessage where

import Maybe

-- local source
import BackgroundState (..)
import CommonState as Common
import Byte (..)
import Result (..)

type alias FromExtensionMessage = { ping      : Maybe ()
                                  , getInputs : Maybe { context : String }
                                  , update    : Maybe { context  : String
                                                      , login    : String
                                                      , password : String
                                                      }
                                  }

type alias ToExtensionMessage = {connectState : Maybe String}

emptyToExtensionMessage = {connectState = Nothing}

decode : FromExtensionMessage -> BackgroundAction
decode message =
    let decode' {ping, getInputs, update} =
        Maybe.oneOf
            [ Maybe.map (\_ -> SetExtAwaitingPing True) ping
            , Maybe.map (set ExtLogin) getInputs
            , Maybe.map (set ExtUpdate) update
            ]
        set constructor d = SetExtAwaitingData (Just (constructor d))
        -- we do a bytestring conversion to check for errors but we just use
        -- the string above as ByteString is just a type-alias
        errOrInputs = Maybe.map (\{context} -> byteString context) message.getInputs
        errOrUpdate = Maybe.map errOrUpdate' message.update
        errOrUpdate' {context, login, password} =
            let bSc = byteString context
                bSl = byteString login
                bSp = byteString password
            in bSc `andThen` (\_ -> bSl) `andThen` (\_ -> bSp) `andThen` (\_ -> Ok ())
    in case errOrUpdate of
        Just (Err err) -> CommonAction (Common.AppendToLog err)
        _              -> case errOrInputs of
            Just (Err err) -> CommonAction (Common.AppendToLog err)
            _              -> Maybe.withDefault NoOp (decode' message)

encode : BackgroundState -> ToExtensionMessage
encode s =
    if s.extAwaitingPing
    then { connectState = case s.common.connected of
                    Common.Connected    -> Just "connected"
                    Common.NotConnected -> Just "disconnected"
                    _                   -> Just "connected"
         }
    else emptyToExtensionMessage
