var emptyMessage = {setLog:null, setConnected:null};
var gui = Elm.fullscreen(Elm.Main, {toGUI: emptyMessage});

chrome.runtime.onMessage.addListener(
    function(request, sender, sendResponse) {
        console.log(request.toGUI);
        if (request.toGUI != null) {
            gui.ports.toGUI.send(request.toGUI);
        }
    }
);
