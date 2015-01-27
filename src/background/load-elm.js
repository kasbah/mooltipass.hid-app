/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var emptyFromDeviceMessage = { setHidConnected : null
                             , receiveCommand  : null
                             , appendToLog     : null
                             };
var emptyFromExtensionMessage  = { ping      : null
                                 , getInputs : null
                                 , update    : null
                                 };
var guiOpen = false;
var extensionId = null;

var elm = Elm.worker(
    Elm.Background,
    { fromGUI       : emptyFromGuiMessage
    , fromDevice    : emptyFromDeviceMessage
    , fromExtension : emptyFromExtensionMessage
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
    if (message.connect !== null) {
        device.connect();
    } else if (message.sendCommand !== null) {
        sendMsg(message.sendCommand);
    }
});

messageHasValue = function (message) {
    var hasValue = false;
    for (var prop in message) {
        hasValue |= message[prop] !== null;
    }
    return hasValue;
}

elm.ports.toExtension.subscribe(function(message) {
    if (extensionId != null && messageHasValue(message)) {
        chrome.runtime.sendMessage(extensionId, message);
    }
});

deviceSendToElm = function (message) {
    var messageWithNulls = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyFromDeviceMessage) {
        if (message.hasOwnProperty(prop)) {
            messageWithNulls[prop] = message[prop];
        } else {
            messageWithNulls[prop] = emptyFromDeviceMessage[prop];
        }
    }
    elm.ports.fromDevice.send(messageWithNulls);
};

extensionSendToElm = function (message) {
    var messageWithNulls = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyFromExtensionMessage) {
        if (message.hasOwnProperty(prop)) {
            messageWithNulls[prop] = message[prop];
        } else {
            messageWithNulls[prop] = emptyFromExtensionMessage[prop];
        }
    }
    elm.ports.fromExtension.send(messageWithNulls);
}

chrome.runtime.onMessageExternal.addListener(function(message, sender, sendResponse)
{
    extensionId = sender.id;
    extensionSendToElm(message);
});

function launch()
{
    chrome.app.window.create('gui/index.html',
        //id takes care of making sure only one is running
        { id:"mooltipass"
        , minWidth: 550
        , minHeight: 600
        }
    );
}

chrome.runtime.onInstalled.addListener(launch);
chrome.runtime.onStartup.addListener(launch);
chrome.app.runtime.onLaunched.addListener(launch);
