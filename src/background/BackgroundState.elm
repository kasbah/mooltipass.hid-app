module BackgroundState where

-- local source
import CommonState as Common
import CommonState (CommonAction,CommonState)

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

