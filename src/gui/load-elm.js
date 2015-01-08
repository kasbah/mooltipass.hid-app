var gui = Elm.fullscreen(Elm.Main, {toGUI: emptyMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toGUI != null) {
            gui.ports.toGUI.send(request.toGUI);
        }
    });

gui.ports.toBackground.subscribe(function(message) {
        chrome.runtime.sendMessage({toBackground: message});
    });

