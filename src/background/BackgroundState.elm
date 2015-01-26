module BackgroundState where

-- Elm standard library
import Maybe
import List ((::))

-- local source
import CommonState as Common
import CommonState (..)
import DevicePacket (..)
import Byte (..)

type alias BackgroundState = { deviceConnected : Bool
                             , deviceVersion   : Maybe MpVersion
                             , currentContext  : ByteString
                             , extAwaitingPing : Bool
                             , extRequest      : ExtensionRequest
                             , common          : CommonState
                             }

default : BackgroundState
default = { deviceConnected = False
          , deviceVersion   = Nothing
          , currentContext  = ""
          , extAwaitingPing = False
          , extRequest      = NoRequest
          , common          = Common.default
          }

type ExtensionRequest =
      ExtWantsCredentials     { context : ByteString }

    | ExtNeedsLogin           { context : ByteString }

    | ExtNeedsPassword        { context : ByteString
                              , login   : ByteString
                              }

    | ExtCredentials          { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNoCredentials

    | ExtWantsToWrite         { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsNewContext      { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsToWriteLogin    { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsToWritePassword { context  : ByteString
                              , password : ByteString
                              }

    | ExtWriteComplete        { context  : ByteString }

    | ExtNotWritten

    | NoRequest

extensionRequestToLog : ExtensionRequest -> Maybe String
extensionRequestToLog d = case d of
    ExtWantsCredentials {context} ->
        Just <| "> requesting credentials for " ++ context
    ExtNeedsToWriteLogin {context, login, password} ->
        Just <| "> writing credentials for " ++ context
    ExtNeedsNewContext {context, login, password} ->
        Just <| "> adding new credentials for " ++ context
    ExtNoCredentials    -> Just "access denied or no credentials"
    ExtNotWritten       -> Just "access denied"
    ExtWriteComplete _  -> Just "credentials written"
    ExtCredentials   _  -> Just "credentials retrieved"
    _ -> Nothing

type BackgroundAction = SetHidConnected    Bool
                      | SetExtAwaitingPing Bool
                      | SetExtensionRequest ExtensionRequest
                      | Receive            DevicePacket
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
               {s | deviceConnected <-  False}
            else {s | deviceConnected <- True}
        SetExtAwaitingPing b -> {s | extAwaitingPing <- b}
        SetExtensionRequest d -> {s | extRequest <- d}
        CommonAction (SetConnected c) ->
            let s' = {s | common <- updateCommon (SetConnected c)}
            in if c /= s.common.connected
               then {s' | common <-
                            Common.update
                                (AppendToLog (connectToLog c))
                                s'.common
                        , currentContext <- ""
                        , deviceVersion  <- Nothing
                    }
               else s'
        CommonAction a -> {s | common <- updateCommon a}
        Receive (DeviceGetLogin ml) -> case ml of
            Just l ->
                {s | extRequest <- case s.extRequest of
                          ExtNeedsLogin c ->
                              ExtNeedsPassword {c | login = l}
                          ExtWantsCredentials c ->
                              ExtNeedsPassword {c | login = l}
                          _ -> NoRequest
                }
            Nothing -> { s | extRequest <- ExtNoCredentials }
        Receive (DeviceGetPassword mp) -> case mp of
            Just p ->
                {s | extRequest <- case s.extRequest of
                          ExtNeedsPassword c ->
                              ExtCredentials {c | password = p}
                          _ -> NoRequest
                }
            Nothing -> { s | extRequest <- ExtNoCredentials }
        Receive (DeviceSetLogin r) ->
            {s | extRequest <- case s.extRequest of
                     ExtNeedsToWriteLogin c ->
                         if r == Done
                         then ExtNeedsToWritePassword { c - login }
                         else ExtNotWritten
                     _ -> NoRequest
            }
        Receive (DeviceSetPassword r) ->
            {s | extRequest <- case s.extRequest of
                     ExtNeedsToWritePassword c ->
                         if r == Done
                         then ExtWriteComplete { c - password }
                         else ExtNotWritten
                     _ -> NoRequest
            }
        Receive (DeviceSetContext r) ->
            case r of
                ContextSet -> case s.extRequest of
                    ExtWantsCredentials c ->
                        {s | currentContext <- c.context
                           , extRequest <- ExtNeedsLogin c
                        }
                    ExtWantsToWrite c ->
                        {s | currentContext <- c.context
                           , extRequest <- ExtNeedsToWriteLogin c
                        }
                    ExtNeedsLogin c ->
                        {s | currentContext <- c.context}
                    ExtNeedsPassword c  ->
                        {s | currentContext <- c.context}
                    ExtNeedsToWriteLogin c ->
                        {s | currentContext <- c.context}
                    ExtNeedsToWritePassword c ->
                        {s | currentContext <- c.context}
                    -- this fall-through would be: we have no idea what context
                    -- we set so we just keep the original state
                    _ -> s
                UnknownContext -> case s.extRequest of
                    ExtWantsToWrite c ->
                        {s | extRequest <- ExtNeedsNewContext c}
                    ExtWantsCredentials _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsLogin _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsPassword _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsToWritePassword _ ->
                        {s | extRequest <- ExtNotWritten}
                    ExtNeedsToWriteLogin _ ->
                        {s | extRequest <- ExtNotWritten}
                    _ -> s
                NoCardForContext ->
                    update (CommonAction (SetConnected NoCard)) s
        Receive (DeviceAddContext r) ->
            {s | extRequest <- case s.extRequest of
                     ExtNeedsNewContext c ->
                         if r == Done
                         then ExtWantsToWrite c
                         else ExtNotWritten
                     _ -> NoRequest
            }
        Receive (DeviceGetStatus st) ->
            update (CommonAction (SetConnected (case st of
                       NeedCard   -> NoCard
                       Locked     -> NoPin
                       LockScreen -> NoPin
                       Unlocked   -> Connected
                    ))) s
        Receive (DeviceGetVersion v) ->
            update
                (appendToLog
                    ("device is "
                        ++ v.version ++ " "
                        ++ toString v.flashMemSize
                        ++ "MBit"))
                {s | deviceVersion <- Just v}
        Receive x ->
            update
                (appendToLog ("Error: received unhandled packet " ++ toString x))
                s
        NoOp -> s

fromPacket :  (Result Error DevicePacket) -> BackgroundAction
fromPacket r = case r of
    Err err -> appendToLog ("HID Error: " ++ err)
    Ok p    -> Receive p

appendToLog s = CommonAction (AppendToLog s)
