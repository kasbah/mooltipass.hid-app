module BackgroundState where

-- Elm standard library
import Maybe
import List ((::))
import List

-- local source
import CommonState as Common
import CommonState (..)
import DevicePacket (..)
import Byte (..)

type alias BackgroundState = { deviceConnected  : Bool
                             , deviceVersion    : Maybe MpVersion
                             , waitingForDevice : Bool
                             , currentContext   : ByteString
                             , extAwaitingPing  : Bool
                             , extRequest       : ExtensionRequest
                             , mediaImport      : MediaImport
                             , common           : CommonState
                             }

default : BackgroundState
default = { deviceConnected  = False
          , deviceVersion    = Nothing
          , waitingForDevice = False
          , currentContext   = ""
          , extAwaitingPing  = False
          , extRequest       = NoRequest
          , mediaImport      = NoMediaImport
          , common           = Common.default
          }

type MediaImport =
      NoMediaImport
    | MediaImportRequested    String
    | MediaImportStart        (List AppPacket)
    | MediaImportStartWaiting (List AppPacket)
    | MediaImport             (List AppPacket)
    | MediaImportWaiting      (List AppPacket)
    | MediaImportError        String
    | MediaImportSuccess

type ExtensionRequest =
      ExtWantsCredentials     { context : ByteString }

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
    ExtWantsToWrite {context, login, password} ->
        Just <| "> requesting to write credentials for " ++ context
    ExtNeedsNewContext {context, login, password} ->
        Just <| "> adding new credentials for " ++ context
    ExtNoCredentials    -> Just "access denied or no credentials"
    ExtNotWritten       -> Just "access denied"
    ExtWriteComplete _  -> Just "credentials written"
    ExtCredentials   _  -> Just "credentials retrieved"
    _ -> Nothing

type BackgroundAction = SetHidConnected    Bool
                      | SetWaitingForDevice Bool
                      | SetExtAwaitingPing Bool
                      | SetExtRequest      ExtensionRequest
                      | SetMediaImport     MediaImport
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
        SetExtRequest d -> {s | extRequest <- d}
        SetMediaImport t -> {s | mediaImport <- t}
        SetWaitingForDevice b -> {s | waitingForDevice <- b}
        CommonAction (SetConnected c) ->
            let s' = {s | common <- updateCommon (SetConnected c)}
            in if c /= s.common.connected
               then {s' | common <-
                            Common.update
                                (AppendToLog (connectToLog c))
                                s'.common
                        , currentContext <- ""
                        , deviceVersion  <- if c == NotConnected
                                            then Nothing
                                            else s.deviceVersion
                    }
               else s
        CommonAction (StartImportMedia p) -> case s.mediaImport of
            NoMediaImport -> {s | mediaImport <- MediaImportRequested p}
            _             -> s
        CommonAction a -> {s | common <- updateCommon a}
        Receive (DeviceGetLogin ml) -> case ml of
            Just l ->
                {s | extRequest <- case s.extRequest of
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
                     ExtWantsToWrite c ->
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
                        {s | currentContext <- c.context}
                    ExtWantsToWrite c ->
                        {s | currentContext <- c.context}
                    ExtNeedsPassword c  ->
                        {s | currentContext <- c.context}
                    ExtNeedsToWritePassword c ->
                        {s | currentContext <- c.context}
                    -- this fall-through would be: we have no idea what
                    -- context we set so we just keep the original state
                    _ -> s
                UnknownContext -> case s.extRequest of
                    ExtWantsToWrite c ->
                        {s | extRequest <- ExtNeedsNewContext c}
                    ExtWantsCredentials _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsPassword _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsToWritePassword _ ->
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
        Receive (DeviceImportMediaStart r) ->
            update (appendToLog "DeviceImportMediaStart")
            { s | mediaImport <- case s.mediaImport of
                MediaImportStartWaiting ps -> if r == Done then MediaImport ps else MediaImportError "Start failed"
                MediaImportWaiting ps -> if r == Done then MediaImport ps else MediaImportError "Start failed"
                _ -> MediaImportError "Received unexpected start import confirmation from device"
            }
        Receive (DeviceImportMedia r) ->
            update (appendToLog "DeviceImportMedia")
            { s | mediaImport <- case s.mediaImport of
                MediaImportWaiting (p::ps) -> if r == Done then MediaImport ps else MediaImportError "Write failed"
                _ -> MediaImportError "Received unexpected imported data confirmation from device"
            }
        Receive (DeviceImportMediaEnd r) ->
            update (appendToLog "DeviceImportMediaEnd")
            { s | mediaImport <- case s.mediaImport of
                MediaImportWaiting [] -> if r == Done then MediaImportSuccess else MediaImportError "End not succeeded"
                _ -> MediaImportError "Received unexpected end import confirmation from device"
            }
        Receive x ->
            update
                (appendToLog ("Error: received unhandled packet " ++ toString x))
                s
        NoOp -> s

fromPacket :  (Result Error DevicePacket) -> BackgroundAction
fromPacket r = case r of
    Err err -> appendToLog ("HID Error: " ++ err)
    Ok p    -> Receive p

apply : List BackgroundAction -> BackgroundState -> BackgroundState
apply actions state = List.foldr update state actions

appendToLog s = CommonAction (AppendToLog s)
