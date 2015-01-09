/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var emptyMpMessage = {appendToLog : ""};
var elm = Elm.worker(Elm.Background, {fromGUI: emptyMessage, fromMP : emptyMpMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toBackground != null) {
            elm.ports.fromGUI.send(request.toBackground);
        }
    });

elm.ports.toGUI.subscribe(function(message) {
        chrome.runtime.sendMessage({toGUI: message});
    });
