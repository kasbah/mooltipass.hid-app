/* This file loads the Elm application and sets up communication with the
   background through chrome.runtime. */

var emptyFromChromeMessage = {pickedMediaFile: null};

var gui = Elm.fullscreen(Elm.GUI,
    { fromBackground: emptyToGuiMessage
    , fromChrome: emptyFromChromeMessage
    }
);

chrome.runtime.onMessage.addListener(function(message, sender, sendResponse) {
    if (message.toGUI !== undefined) {
        gui.ports.fromBackground.send(message.toGUI);
    }
});

//get the current state
var getStateMessage = {};
for (var prop in emptyFromGuiMessage) {
    if (prop != "getState")
        getStateMessage[prop] = emptyFromGuiMessage[prop];
}
getStateMessage["getState"] = [];
chrome.runtime.sendMessage({
    toBackground:getStateMessage
});

gui.ports.toBackground.subscribe(function(message) {
    if (messageHasValue(message)) {
        chrome.runtime.sendMessage({toBackground: message});
    }
});

gui.ports.toChrome.subscribe(function(message) {
    if (message.pickMediaFile !== null) {
        chrome.fileSystem.chooseEntry({type: 'openFile'}, function(entry) {
            if (entry != null) {
                var id = chrome.fileSystem.retainEntry(entry);
                gui.ports.fromChrome.send({pickedMediaFile: id});
            }
        });
    }
});
