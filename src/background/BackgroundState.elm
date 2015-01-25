module BackgroundState where

-- Elm standard library
import Maybe
import List ((::))

-- local source
import CommonState as Common
import CommonState (..)
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

type ExtData = ExtInputs {context : ByteString}
             | ExtNeedsLogin { context : ByteString }
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
             | ExtAddNew { context  : ByteString
                         , login    : ByteString
                         , password : ByteString
                         }
             | ExtUpdateLogin { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }
             | ExtUpdatePassword { context  : ByteString
                                 , password : ByteString
                                 }
             | ExtUpdateComplete { context  : ByteString }
             | ExtNoCredentials
             | ExtNoUpdate
             | NoData

extDataToLog : ExtData -> Maybe String
extDataToLog d = case d of
    ExtInputs {context} ->
        Just <| "> requesting credentials for " ++ context
    ExtUpdateLogin {context, login, password} ->
        Just <| "> writing credentials for " ++ context
    ExtAddNew {context, login, password} ->
        Just <| "> adding new credentials for " ++ context
    ExtNoCredentials    -> Just "access denied or no credentials"
    ExtNoUpdate         -> Just "access denied"
    ExtUpdateComplete _ -> Just "credentials written"
    ExtCredentials    _ -> Just "credentials retrieved"
    _ -> Nothing

type BackgroundAction = SetHidConnected    Bool
                      | SetExtAwaitingPing Bool
                      | SetExtAwaitingData ExtData
                      | ReceiveGetLogin    String
                      | ReceiveGetPassword String
                      | ReceiveSetLogin    ReturnCode
                      | ReceiveSetPassword ReturnCode
                      | ReceiveSetContext  SetContextReturn
                      | ReceiveAddContext  ReturnCode
                      | CommonAction       CommonAction
                      | NoOp

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b ->
            if not b
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
                                (AppendToLog (connectToLog c))
                                s'.common
                        , currentContext <- ""
                    }
               else s'
        CommonAction a -> {s | common <- updateCommon a}
        ReceiveGetLogin l ->
            {s | extAwaitingData <- case s.extAwaitingData of
                      ExtNeedsLogin c ->
                          ExtNeedsPassword {c | login = l}
                      ExtInputs c ->
                          ExtNeedsPassword {c | login = l}
                      _ -> NoData
            }
        ReceiveGetPassword p ->
            {s | extAwaitingData <- case s.extAwaitingData of
                      ExtNeedsPassword c ->
                          ExtCredentials {c | password = p}
                      _ -> NoData
            }
        ReceiveSetLogin r ->
            {s | extAwaitingData <- case s.extAwaitingData of
                     ExtUpdateLogin c ->
                         if r == Done
                         then ExtUpdatePassword { c - login }
                         else ExtNoUpdate
                     _ -> NoData
            }
        ReceiveSetPassword r ->
            {s | extAwaitingData <- case s.extAwaitingData of
                     ExtUpdatePassword c ->
                         if r == Done
                         then ExtUpdateComplete { c - password }
                         else ExtNoUpdate
                     _ -> NoData
            }
        ReceiveSetContext r ->
            case r of
                ContextSet -> case s.extAwaitingData of
                    ExtInputs c ->
                        {s | currentContext <- c.context
                           , extAwaitingData <- ExtNeedsLogin c
                        }
                    ExtNeedsLogin c ->
                        {s | currentContext <- c.context}
                    ExtNeedsPassword c  ->
                        {s | currentContext <- c.context}
                    ExtUpdate c ->
                        {s | currentContext <- c.context
                           , extAwaitingData <- ExtUpdateLogin c
                        }
                    ExtUpdateLogin c ->
                        {s | currentContext <- c.context}
                    ExtUpdatePassword c ->
                        {s | currentContext <- c.context}
                    -- this fall-through would be: we have no idea what context
                    -- we set so we just keep the original state
                    _ -> s
                UnknownContext -> case s.extAwaitingData of
                    ExtInputs _ ->
                        {s | extAwaitingData <- ExtNoCredentials}
                    ExtNeedsLogin _ ->
                        {s | extAwaitingData <- ExtNoCredentials}
                    ExtNeedsPassword _ ->
                        {s | extAwaitingData <- ExtNoCredentials}
                    ExtUpdate c ->
                        {s | extAwaitingData <- ExtAddNew c}
                    ExtUpdatePassword _ ->
                        {s | extAwaitingData <- ExtNoUpdate}
                    ExtUpdateLogin _ ->
                        {s | extAwaitingData <- ExtNoUpdate}
                    _ -> s
                NoCardForContext ->
                    update (CommonAction (SetConnected NoCard)) s
        ReceiveAddContext r ->
            {s | extAwaitingData <- case s.extAwaitingData of
                     ExtAddNew c ->
                         if r == Done
                         then ExtUpdate c
                         else ExtNoUpdate
                     _ -> NoData
            }
        NoOp -> s


fromPacket :  (Result Error DevicePacket) -> BackgroundAction
fromPacket r = case r of
    Err err -> CommonAction (AppendToLog ("HID Error: " ++ err))
    Ok p    -> case p of
        DeviceGetStatus s -> CommonAction (SetConnected (case s of
                                NeedCard   -> NoCard
                                Locked     -> NoPin
                                LockScreen -> NoPin
                                Unlocked   -> Connected
                             ))
        DeviceGetLogin ms    ->
            Maybe.withDefault
                (SetExtAwaitingData ExtNoCredentials)
                (Maybe.map ReceiveGetLogin ms)
        DeviceGetPassword ms ->
            Maybe.withDefault
                (SetExtAwaitingData ExtNoCredentials)
                (Maybe.map ReceiveGetPassword ms)
        DeviceSetLogin    r  -> ReceiveSetLogin r
        DeviceSetPassword r  -> ReceiveSetPassword r
        DeviceSetContext r   -> ReceiveSetContext r
        DeviceAddContext r   -> ReceiveAddContext r
        _                    -> NoOp
