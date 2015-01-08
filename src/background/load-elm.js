/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */

var bg = Elm.worker(Elm.Background, {fromGUI: emptyMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toBackground != null) {
            bg.ports.fromGUI.send(request.toBackground);
        }
    });

bg.ports.toGUI.subscribe(function(message) {
        chrome.runtime.sendMessage({toGUI: message});
    });
