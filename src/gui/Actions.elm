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

sendParameter : Parameter -> Byte -> Message
sendParameter p b = send guiActions (CommonAction (SetParameter (Just (p, b))))

sendIntContent : Parameter -> Int -> Int -> Content -> Message
sendIntContent p lo hi content = send guiActions (SetParameterField p lo hi content)

sendParseInt : Parameter -> String -> Message
sendParseInt p s = case toInt s of
  Ok i -> sendParameter p i
  _    -> send guiActions NoOp

sendBool : Parameter -> Bool -> Message
sendBool p b = case b of
  True  -> sendParameter p 1
  False -> sendParameter p 0

setKeyboard : Int -> Message
setKeyboard kb = sendParameter KeyboardLayout kb
