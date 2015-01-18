/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var emptyFromDeviceMessage = { setConnected   : null
                             , receiveCommand : null
                             , appendToLog    : null
                             };
var elm = Elm.worker(
    Elm.Background,
    { fromGUI    : emptyFromGuiMessage
    , fromDevice : emptyFromDeviceMessage
    }
);

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
    if (request.toBackground !== undefined) {
        elm.ports.fromGUI.send(request.toBackground);
    }
});

elm.ports.toGUI.subscribe(function(message) {
    chrome.runtime.sendMessage({toGUI: message});
});

elm.ports.toDevice.subscribe(function(message) {
    if (message.checkConnection != null) {
        device.checkConnection();
    }
});

sendToElm = function (message) {
    var messageWithNulls = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyFromDeviceMessage) {
        if(message.hasOwnProperty(prop)){
            messageWithNulls[prop] = message[prop];
        } else {
            messageWithNulls[prop] = emptyFromDeviceMessage[prop];
        }
    }
    elm.ports.fromDevice.send(messageWithNulls);
};

chrome.runtime.onMessageExternal.addListener(function(request, sender, sendResponse)
{
    console.log('received request '+request.type);
});

function launch()
{
    chrome.app.window.create('gui/index.html', {minWidth: 550, minHeight: 600});
}

//chrome.runtime.onInstalled.addListener(launch);
chrome.app.runtime.onLaunched.addListener(launch);

sendToElm({setConnected:"NotConnected"});
