module ExtensionMessage where

import Maybe

-- local source
import BackgroundState (..)

type alias FromExtensionMessage = { ping      : Maybe ()
                                  , getInputs : Maybe Context
                                  , update    : Maybe (Context, String, String)
                                  }
type alias ToExtensionMessage = {connectState : Maybe String}

decode : FromExtensionMessage -> BackgroundAction
decode message =
    let decode' {ping, getInputs, update} =
        Maybe.oneOf
            [ Maybe.map (\_ -> SetExtAwaitingPing True) ping
            , Maybe.map (set Inputs) getInputs
            , Maybe.map (set Update) update
            ]
        set constructor d =  SetExtAwaitingData (Just (constructor d))
    in Maybe.withDefault NoOp (decode' message)
