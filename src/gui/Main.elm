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

forBg : GuiState -> (FromGuiMessage, Action, GuiState)
forBg s =
    let e = emptyFromGuiMessage
        mImportRequested = case s.importMedia of
            RequestFile _ -> True
            _ -> False
        memManage = case s.unsavedMemInfo of
            Common.MemInfoRequest -> True
            Common.MemInfoSave  _ -> True
            _                     -> False

        getStringCmd cmd = FromGuiMessage.encode (Common.GetStringCmd (Just cmd))
        (doNeedStringCmd, needSC, needCmd) = case List.take 1 s.needStringCmds of
            [c]  -> (True, getStringCmd c, Just c)
            []   -> (False, emptyFromGuiMessage, Nothing)
        (doGetCmd, encGetCmd)  = case s.getStringCmd of
            Just c  -> (True, getStringCmd c)
            Nothing -> (False, emptyFromGuiMessage)

        setParam pb = FromGuiMessage.encode (Common.SetParameter (Just pb))
        (doSaveParam, needSet, saveParam) = case List.take 1 s.saveParameters of
            [pb] -> (True, setParam pb, Just pb)
            []   -> (False, emptyFromGuiMessage, Nothing)
        (doSetParam, encSet) = case s.setParameter of
            Just pb -> (True, setParam pb)
            Nothing -> (False, emptyFromGuiMessage)

        getParam p = FromGuiMessage.encode (Common.GetParameter (Just p))
        (doNeedParam, needGet, needParam) = case List.take 1 s.needParameters of
            [p]  -> (True, getParam p, Just p)
            []   -> (False, emptyFromGuiMessage, Nothing)
        (doGetParam, encGet)  = case s.getParameter of
            Just p  -> (True, getParam p)
            Nothing -> (False, emptyFromGuiMessage)
    in if
        | mImportRequested -> case s.importMedia of
            RequestFile p ->
                ( FromGuiMessage.encode (Common.StartImportMedia p)
                , SetImportMedia NotRequested, s)
            _             -> (e, NoOp, s)
        | memManage -> case s.unsavedMemInfo of
                Common.MemInfoRequest ->
                        (FromGuiMessage.encode Common.StartMemManage
                        , SetUnsavedMem Common.MemInfoWaitingForUser, s)
                Common.MemInfoSave d ->
                        (FromGuiMessage.encode (Common.SaveMemManage d)
                        , SetUnsavedMem (Common.MemInfo d), s)
        | doSetParam -> (encSet, NoOp, {s | setParameter <- Nothing})
        | doGetCmd   -> (encGetCmd, NoOp, {s | getStringCmd <- Nothing})
        | doGetParam -> (encGet, NoOp, {s | getParameter <- Nothing})
        | doSaveParam -> (needSet, NoOp,
               {s | saveParameters <- List.drop 1 s.saveParameters, setParameter <- saveParam})
        | doNeedStringCmd -> (needSC, NoOp,
               {s | needStringCmds <- List.drop 1 s.needStringCmds, getStringCmd <- needCmd})
        | doNeedParam -> (needGet, NoOp,
               {s | needParameters <- List.drop 1 s.needParameters, getParameter <- needParam})
 
        | otherwise -> (e, NoOp, s)

output : Signal (ToChromeMessage, FromGuiMessage, List Int, GuiState)
output =
    let go ias (_,_,_,s) =
        let s1            = apply ias s
            (tcm, a1)     = ChromeMessage.encode s1
            s2            = update a1 s1
            (fgm, a2, s3) = forBg s2
            (is, a3)      = forDevice s3
            s4            = update a3 s3
        in (tcm, fgm, is, update a2 s4)
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
