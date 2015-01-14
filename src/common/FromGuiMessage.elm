module FromGuiMessage where

-- local source
import CommonState (..)

type alias FromGuiMessage = { setLog : Maybe (List String)
                            }

encode : CommonAction -> FromGuiMessage
encode action =
        case action of
            SetLog s -> {setLog = Just s}
            _        -> {setLog = Nothing}

decode :  FromGuiMessage -> CommonAction
decode msg = case msg.setLog of
        Just s -> SetLog s
        Nothing -> CommonNoOp
