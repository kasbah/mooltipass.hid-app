/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var emptyMpMessage = {appendToLog: null, setConnected: null};
var elm = Elm.worker(Elm.Background, {fromGUI: emptyFromGuiMessage, fromMP: emptyMpMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toBackground != null) {
            elm.ports.fromGUI.send(request.toBackground);
        }
    });

elm.ports.toGUI.subscribe(function(message) {
        chrome.runtime.sendMessage({toGUI: message});
    });

sendToElm = function (obj) {
    var msg = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyMpMessage) {
        if(obj.hasOwnProperty(prop)){
            msg[prop] = obj[prop];
        } else {
            msg[prop] = emptyMpMessage[prop];
        }
    }
    elm.ports.fromMP.send(msg);
};
