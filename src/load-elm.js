var gui = Elm.fullscreen(Elm.Main, {toGUI: ""});

chrome.runtime.onMessage.addListener(
    function(request, sender, sendResponse) {
        if (request.type == 'toGUI') {
            gui.ports.toGUI.send(request.data);
        }
    }
);
