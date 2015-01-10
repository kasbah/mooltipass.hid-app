/* This file loads the Elm application and sets up communication with the
   background through chrome.runtime. */

var gui = Elm.fullscreen(Elm.GUI, {fromBackground: emptyGuiMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toGUI != null) {
            gui.ports.fromBackground.send(request.toGUI);
        }
    });

gui.ports.toBackground.subscribe(function(message) {
        chrome.runtime.sendMessage({toBackground: message});
    });

