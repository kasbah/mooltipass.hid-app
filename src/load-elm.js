var gui = Elm.fullscreen(Elm.Main, {appendToLog: ""});

function appendToLog(str) {
    gui.ports.appendToLog.send(str);
}
