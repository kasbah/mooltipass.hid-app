module Background where

-- Elm standard library
import Signal (..)
import Graphics.Element (..)
import Time (every, second)
import List
import List ((::))
import Maybe

-- local source
import ToGuiMessage
import ToGuiMessage (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage (FromGuiMessage)
import CommonState as Common
import CommonState (CommonAction,CommonState)

type alias FromDeviceMessage = { setHidConnected : Maybe Bool
                               , receiveCommand  : Maybe (List Int)
                               , appendToLog     : Maybe String
                               }

type alias ToDeviceMessage   = { connect     : Maybe ()
                               , sendCommand : Maybe (List Int)
                               }
emptyToDeviceMessage = {connect = Nothing, sendCommand = Nothing}

type alias BackgroundState = {hidConnected : Bool, common : CommonState}

default : BackgroundState
default = {hidConnected = False, common = Common.default}

type BackgroundAction = SetHidConnected Bool
                      | CommonAction CommonAction
                      | NoOp

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b -> {s | hidConnected <- b}
        CommonAction a    -> {s | common <- updateCommon a}
        NoOp              -> s

decode : FromDeviceMessage -> BackgroundAction
decode message =
    let decode {setHidConnected, receiveCommand, appendToLog} =
        Maybe.oneOf
            [ Maybe.map (CommonAction << Common.AppendToLog) appendToLog
            , Maybe.map SetHidConnected setHidConnected
            ]
    in Maybe.withDefault NoOp (decode message)

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = (ToGuiMessage.encode << .common) <~ state

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice = constant {connect = Nothing, sendCommand = Nothing}

state : Signal BackgroundState
state =
    foldp update default
        <| merge (decode <~ fromDevice)
        <| CommonAction <~ (FromGuiMessage.decode <~ fromGUI)
