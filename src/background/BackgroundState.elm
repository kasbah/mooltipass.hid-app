module BackgroundState where

-- Elm standard library
import Maybe
import List ((::))

-- local source
import CommonState as Common
import CommonState (CommonAction(..),CommonState,ConnectState (..), toLogString)
import DevicePacket (..)

type alias BackgroundState = { hidConnected    : Bool
                             , currentContext  : ByteString
                             , extAwaitingPing : Bool
                             , extAwaitingData : ExtData
                             , common          : CommonState
                             }

default : BackgroundState
default = { hidConnected    = False
          , currentContext  = ""
          , extAwaitingPing = False
          , extAwaitingData = NoData
          , common          = Common.default
          }

type ExtData = ExtNeedsLogin {context : ByteString}
             | ExtNeedsPassword { context : ByteString
                                , login   : ByteString
                                }
             | ExtCredentials { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }
             | ExtUpdate { context  : ByteString
                         , login    : ByteString
                         , password : ByteString
                         }
             | ExtUpdatePassword { context  : ByteString
                                 , password : ByteString
                                 }
             | ExtUpdateComplete { context  : ByteString }
             | ExtNoCredentials
             | NoData

type BackgroundAction = SetHidConnected Bool
                      | SetExtAwaitingPing Bool
                      | SetExtAwaitingData ExtData
                      | GotLogin String
                      | GotPassword String
                      | SetLogin ReturnCode
                      | SetPassword ReturnCode
                      | SetContext SetContextReturn
                      | CommonAction CommonAction
                      | NoOp

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b -> if not b
                             then update
                                (CommonAction (SetConnected NotConnected))
                                {s | hidConnected <-  False}
                             else {s | hidConnected <- True}
        SetExtAwaitingPing b -> {s | extAwaitingPing <- b}
        SetExtAwaitingData d -> {s | extAwaitingData <- d}
        CommonAction (SetConnected c) ->
            let s' = {s | common <- updateCommon (SetConnected c)}
            in if c /= s.common.connected
               then {s' | common <-
                            Common.update
                                (AppendToLog (toLogString c))
                                s'.common
                    }
               else s'
        CommonAction a -> {s | common <- updateCommon a}
        GotLogin     l -> {s | extAwaitingData <- case s.extAwaitingData of
                                    ExtNeedsLogin c ->
                                        ExtNeedsPassword {c | login = l}
                                    _ -> s.extAwaitingData
                          }
        GotPassword  p -> {s | extAwaitingData <- case s.extAwaitingData of
                                    ExtNeedsPassword c ->
                                        ExtCredentials {c | password = p}
                                    _ -> s.extAwaitingData
                          }
        SetLogin  r    -> {s | extAwaitingData <- case s.extAwaitingData of
                                       ExtUpdate c ->
                                           ExtUpdatePassword { c - login }
                                       _ -> s.extAwaitingData
                          }
        SetPassword  r -> {s | extAwaitingData <- case s.extAwaitingData of
                                    ExtUpdatePassword c ->
                                        ExtUpdateComplete { c - password }
                                    _ -> s.extAwaitingData
                          }
        SetContext r   -> case r of
                            ContextSet -> case s.extAwaitingData of
                                ExtNeedsLogin c     ->
                                    {s | currentContext <- c.context}
                                ExtNeedsPassword c  ->
                                    {s | currentContext <- c.context}
                                ExtUpdate c         ->
                                    {s | currentContext <- c.context}
                                ExtUpdatePassword c ->
                                    {s | currentContext <- c.context}
                                _                          -> s
                            UnknownContext -> case s.extAwaitingData of
                                ExtNeedsLogin _     ->
                                    {s | extAwaitingData <- ExtNoCredentials}
                                ExtNeedsPassword _  ->
                                    {s | extAwaitingData <- ExtNoCredentials}
                                ExtUpdate _         ->
                                    {s | extAwaitingData <- NoData}
                                ExtUpdatePassword _ ->
                                    {s | extAwaitingData <- NoData}
                                _                          -> s
                            NoCardForContext -> update (CommonAction (AppendToLog "NO CARD FOR CONTEXT HAPPENED!!!111!!!")) s
        NoOp           -> s


fromPacket :  (Result Error DevicePacket) -> BackgroundAction
fromPacket r = case r of
    Err err -> CommonAction (AppendToLog ("HID Error: " ++ err))
    Ok p    -> case p of
        DeviceGetStatus s -> CommonAction (SetConnected (case s of
                                NeedCard   -> Common.NoCard
                                Locked     -> Common.NoPin
                                LockScreen -> Common.NoPin
                                Unlocked   -> Common.Connected
                             ))
        DeviceGetLogin ms    ->
            Maybe.withDefault
                (SetExtAwaitingData ExtNoCredentials)
                (Maybe.map GotLogin ms)
        DeviceGetPassword ms ->
            Maybe.withDefault
                (SetExtAwaitingData ExtNoCredentials)
                (Maybe.map GotPassword ms)
        DeviceSetLogin    r  -> SetLogin r
        DeviceSetPassword r  -> SetPassword r
        DeviceSetContext r   -> SetContext r
        _                    -> NoOp
