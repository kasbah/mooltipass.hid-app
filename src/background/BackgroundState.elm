module BackgroundState where

-- local source
import CommonState as Common
import CommonState (CommonAction,CommonState)

type alias Context = String

type alias BackgroundState = { hidConnected    : Bool
                             , extAwaitingPing : Bool
                             , extAwaitingData : Maybe ExtData
                             , common          : CommonState
                             }

default : BackgroundState
default = { hidConnected    = False
          , extAwaitingPing = False
          , extAwaitingData = Nothing
          , common          = Common.default
          }

type ExtData = Inputs Context | Update Context

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

