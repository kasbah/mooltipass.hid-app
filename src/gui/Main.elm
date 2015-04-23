module GUI where
-- Elm standard library
import Graphics.Element (..)
import Signal (..)
import Time (..)
import Window
import List
import Maybe

-- local source
import Scene
import ToGuiMessage
import ToGuiMessage (..)
import FromGuiMessage
import FromGuiMessage (..)
import Actions (..)
import GuiState (..)
import ChromeMessage (..)
import ChromeMessage
import CommonState as Common
import CommonState(MemInfo(..), CommonAction(..))
import DevicePacket (..)

import Debug (log)

{-| Any state updates from the background are received through this port -}
port fromBackground : Signal ToGuiMessage

{-| Any actions to the common state are first passed to the background. They
    should bubble back up throught the 'port fromBackground'. -}
port toBackground : Signal FromGuiMessage
port toBackground =
    merge
        (map (\(_,m,_,_) -> m) output)
        (map FromGuiMessage.encode (subscribe commonActions))

port toChrome : Signal ToChromeMessage
port toChrome = map (\(m,_,_,_) -> m) output

port fromChrome : Signal FromChromeMessage

port toDevice : Signal (List Int)
port toDevice = map (\(_,_,s,_) -> s) output

port fromDevice : Signal (List Int)

{-| The complete application state signal to map our main element to. It is
    the gui-state updated by any state updates from the background. -}
state : Signal GuiState
state = map (\(_,_,_,s) -> s) output

forDevice : GuiState -> (List Int, Action)
forDevice s =
    case s.unsavedMemInfo of
        MemInfoUnknownCardInserted ->
            (toInts OutgoingGetCardCpz
            , CommonAction (SetMemInfo MemInfoUnknownCardWaitingForCpz))
        MemInfoUnknownCardAdd card ->
            (toInts (OutgoingAddNewCard card)
            , NoOp)
        _ -> ([],NoOp)

forBg : GuiState -> (FromGuiMessage, Action)
forBg s =
    let e = emptyFromGuiMessage
        mImportRequested = case s.importMedia of
            RequestFile _ -> True
            _ -> False
        memManage = case s.unsavedMemInfo of
            Common.MemInfoRequest -> True
            Common.MemInfoSave  _ -> True
            _                     -> False
{-
        setKb = case s.wantSetKeyboard of
            Just _  -> True
            Nothing -> False
        kb' = Maybe.withDefault 0 s.wantSetKeyboard
-}
        (setParam, encSet) = case s.setParameter of
            Just (p,b) -> (True, FromGuiMessage.encode (Common.SetParameter (Just (p, b))))
            Nothing -> (False, emptyFromGuiMessage)
        (getParam, encGet)  = case s.getParameter of
            Just p  -> (True, FromGuiMessage.encode (Common.GetParameter (Just p)))
            Nothing -> (False, emptyFromGuiMessage)
    in if
        | mImportRequested -> case s.importMedia of
            RequestFile p ->
                ( FromGuiMessage.encode (Common.StartImportMedia p)
                , SetImportMedia NotRequested)
            _             -> (e, NoOp)
        | memManage -> case s.unsavedMemInfo of
                Common.MemInfoRequest ->
                        (FromGuiMessage.encode Common.StartMemManage
                        , SetUnsavedMem Common.MemInfoWaitingForUser)
                Common.MemInfoSave d ->
                        (FromGuiMessage.encode (Common.SaveMemManage d)
                        , SetUnsavedMem (Common.MemInfo d))
{-
        | setKb -> log ("gui.Main: WANT SET KB to " ++ toString kb') <| (FromGuiMessage.encode (Common.SetKeyboard kb'), NoOp)
-}
        | setParam -> (encSet, NoOp)
        
        | getParam -> log ("gui.Main: WANT GET KB") <| (encGet, NoOp)
        | otherwise -> (e, NoOp)

output : Signal (ToChromeMessage, FromGuiMessage, List Int, GuiState)
output =
    let go ias (_,_,_,s) =
        let s'        = apply ias s
            (tcm, a1) = ChromeMessage.encode s'
            s''       = update a1 s'
            (fgm, a2) = forBg s''
            (is, a3)  = forDevice s''
            s'''      = update a3 s''
        in (tcm, fgm, is, update a2 s''')
    in foldp go
        (emptyToChromeMessage, emptyFromGuiMessage, [], default)
        inputActions

{-| Our main function simply maps the scene to the window dimensions and state
    signals. The scene converts a state and window dimension into an Element.
-}
main : Signal Element
main = Scene.scene <~ Window.dimensions ~ state

inputActions : Signal (List Action)
inputActions = mergeMany
    [ map ((List.map CommonAction) << ToGuiMessage.decode) fromBackground
    , map (\m -> [m]) (subscribe guiActions)
    , map ((\m -> [m]) << ChromeMessage.decode) fromChrome
    , map ((\m -> [m]) << fromResult << fromInts) fromDevice
    ]

fromResult :  Result String ReceivedPacket -> Action
fromResult r = case r of
    Err err -> CommonAction (AppendToLog ("HID Error: " ++ err))
    Ok p    -> Interpret p
