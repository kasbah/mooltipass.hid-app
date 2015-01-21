module BackgroundState where

-- Elm standard library
import Maybe

-- local source
import CommonState as Common
import CommonState (CommonAction,CommonState)
import DevicePacket (..)

type alias BackgroundState = { hidConnected    : Bool
                             , currentContext  : ByteString
                             , extAwaitingPing : Bool
                             , extAwaitingData : Maybe ExtData
                             , common          : CommonState
                             }

default : BackgroundState
default = { hidConnected    = False
          , currentContext  = ""
          , extAwaitingPing = False
          , extAwaitingData = Nothing
          , common          = Common.default
          }

type ExtData = ExtLogin {context : ByteString}
             | ExtPassword { context : ByteString
                           , login   : ByteString
                           }
             | ExtUpdate { context  : ByteString
                         , login    : ByteString
                         , password : ByteString
                         }
             | ExtUpdatePassword { context  : ByteString
                                 , password : ByteString
                                 }

type BackgroundAction = SetHidConnected Bool
                      | SetExtAwaitingPing Bool
                      | SetExtAwaitingData (Maybe ExtData)
                      | CommonAction CommonAction
                      | NoOp

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b    -> {s | hidConnected <- b}
        SetExtAwaitingPing b -> {s | extAwaitingPing <- b}
        SetExtAwaitingData d -> {s | extAwaitingData <- d}
        CommonAction a       -> {s | common <- updateCommon a}
        NoOp                 -> s

toPacket : BackgroundState -> Maybe AppPacket
toPacket s =
    let cc = s.currentContext
        convert data = case data of
            ExtLogin {context} ->
                if cc == context then AppGetLogin
                else AppSetContext context
            ExtPassword {context, login} ->
                if cc == context then AppGetPassword
                else AppSetContext context
            ExtUpdate {context, login, password} ->
                if cc == context then AppSetLogin login
                else AppSetContext context
            ExtUpdatePassword {context, password} ->
                if cc == context then AppSetPassword password
                else AppSetContext context
    in Maybe.map convert s.extAwaitingData

fromPacket :  (Result Error DevicePacket) -> BackgroundAction
fromPacket r = case r of
    Err err -> CommonAction (Common.AppendToLog err)
    Ok p    -> case p of
        DeviceGetStatus s -> CommonAction (Common.SetConnected (case s of
                                NeedCard   -> Common.NoCard
                                Locked     -> Common.NoPin
                                LockScreen -> Common.NoPin
                                Unlocked   -> Common.Connected
                             ))
        _                 -> NoOp
