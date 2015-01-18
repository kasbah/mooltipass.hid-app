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
import CommonState (..)

type alias FromDeviceMessage = { setConnected    : Maybe String
                               , receiveCommand  : Maybe (List Int)
                               , appendToLog     : Maybe String
                               }

type alias ToDeviceMessage   = { checkConnection : Maybe ()
                               , sendCommand     : Maybe (List Int)
                               }
emptyToDeviceMessage = {checkConnection = Nothing, sendCommand = Nothing}

mpDecode : FromDeviceMessage -> CommonAction
mpDecode message =
    let decode {setConnected, receiveCommand, appendToLog} =
        Maybe.oneOf [ Maybe.map AppendToLog appendToLog
                    , Maybe.map connectedFromString setConnected
                    ]
        connectedFromString s =
            case s of
                "NotConnected" -> SetConnected NotConnected
                "Connected"    -> SetConnected Connected
                "NoCard"       -> SetConnected NoCard
                "NoPin"        -> SetConnected NoPin
                _              -> CommonNoOp
    in Maybe.withDefault CommonNoOp (decode message)

port fromGUI : Signal FromGuiMessage

port fromDevice : Signal FromDeviceMessage

port toDevice : Signal ToDeviceMessage
port toDevice =
    (\_ -> {emptyToDeviceMessage | checkConnection <- Just ()}) <~ every (2*second)

state : Signal CommonState
state =
    foldp update default
        <| merge (mpDecode <~ fromDevice)
        <| FromGuiMessage.decode <~ fromGUI

port toGUI : Signal ToGuiMessage
port toGUI = ToGuiMessage.encode <~ state

--deviceActions : Channel (List Int)
--deviceActions = channel []
--
--port toDevice : Signal (List Int)
--port toDevice = subscribe deviceActions

main : Signal Element
main = constant empty
