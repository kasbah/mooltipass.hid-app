/* This file loads the Elm application and sets up communication with the
   background through chrome.runtime. */


var gui = Elm.fullscreen(Elm.GUI, {fromBackground: emptyToGuiMessage});

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
            console.log(entry);
        });
    }
});
