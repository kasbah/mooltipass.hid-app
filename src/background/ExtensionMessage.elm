module ExtensionMessage where

-- local source
import BackgroundState (..)

type alias FromExtensionMessage = { ping : Maybe ()
                                  , getInputs : Maybe String
                                  , update : Maybe (String, String, String)
                                  }
type alias ToExtensionMessage = {connectState : Maybe String}

decode : FromExtensionMessage -> BackgroundAction
decode msg = NoOp
