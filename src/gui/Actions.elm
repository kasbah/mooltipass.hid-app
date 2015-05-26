module Actions where

-- Elm standard library
import List
import Graphics.Input.Field (Content)
import Signal (..)

import Byte (..)
import String (toInt)
import CommonState (..)
import GuiState (..)

{-| The channel that user inputs can 'Signal.send' actions to that affect the
    GUI directly -}
guiActions : Channel Action
guiActions = channel NoOp

{-| The channel that user inputs can 'Signal.send' actions to that affect the common state with the background. These are sent to the background first before they bubble back up to the 'GuiState'. -}
commonActions : Channel CommonAction
commonActions = channel CommonNoOp

sendGetParameter : Parameter -> Message
sendGetParameter p = send guiActions (CommonAction (GetParameter (Just p)))

stageParameter : Parameter -> Byte -> Message
stageParameter p b = send guiActions (StageParameter (p, b))

stageIntContent : Parameter -> Int -> Int -> Content -> Message
stageIntContent p lo hi content = send guiActions (StageParameterField p lo hi content)

stageParseInt : Parameter -> String -> Message
stageParseInt p s = case toInt s of
  Ok i -> stageParameter p i
  _    -> send guiActions NoOp

stageBool : Parameter -> Bool -> Message
stageBool p b = case b of
  True  -> stageParameter p 1
  False -> stageParameter p 0

stageKeyboard : Int -> Message
stageKeyboard kb = stageParameter KeyboardLayout kb
