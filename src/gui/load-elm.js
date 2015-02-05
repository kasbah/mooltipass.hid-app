/* This file loads the Elm application and sets up communication with the
   background through chrome.runtime. */

var emptyFromChromeMessage = {pickedMediaFile: null};


var gui = Elm.fullscreen(Elm.GUI,
    { fromBackground: emptyToGuiMessage
    , fromChrome: emptyFromChromeMessage
    }
);

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
    if (request.toGUI !== undefined) {
        gui.ports.fromBackground.send(request.toGUI);
    }
});

//get the current state
chrome.runtime.sendMessage({toBackground:{setLog:null, getState:[], startImportMedia:null}});

gui.ports.toBackground.subscribe(function(message) {
    chrome.runtime.sendMessage({toBackground: message});
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
