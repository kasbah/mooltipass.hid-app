module FromGuiMessage where

-- local source
import CommonState (..)

type alias FromGuiMessage = { setLog : Maybe (List String)
                            , getState : Maybe ()
                            }

encode : CommonAction -> FromGuiMessage
encode action =
        case action of
            SetLog s -> {setLog = Just s , getState = Nothing}
            GetState -> {setLog = Nothing, getState = Just ()}
            _        -> {setLog = Nothing, getState = Nothing}

decode :  FromGuiMessage -> CommonAction
decode msg = case msg.setLog of
        Just s -> SetLog s
        Nothing -> CommonNoOp
