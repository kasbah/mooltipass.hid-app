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
var guiOpen     = false;
var extensionId = null;
var readingFile = false;

var elm = Elm.worker(
    Elm.Background,
    { fromGUI       : emptyFromGuiMessage
    , fromDevice    : emptyFromDeviceMessage
    , fromExtension : emptyFromExtensionMessage
    , fromChrome    : {readFile: []}
    , deviceStatus  : 0
    }
);

chrome.runtime.onMessage.addListener(function(message, sender, sendResponse) {
    if (message.toBackground !== undefined) {
        elm.ports.fromGUI.send(message.toBackground);
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

elm.ports.toChrome.subscribe(function(message) {
    if (message.readFile !== null && (! readingFile)) {
        readingFile = true;
        chrome.fileSystem.restoreEntry(message.readFile, function(entry) {
            entry.file(function(file) {
                var reader = new FileReader();
                reader.onerror = function(e) {
                    console.log(e);
                    readingFile = false;
                };
                reader.onloadend = function(e) {
                    var bytes = new Uint8Array(reader.result);
                    var ints = [];
                    var pages = [];
                    var packets = [];
                    for (var i = 0, len = bytes.length; i < len; i++) {
                        ints[i] = bytes[i];
                    }
                    for (var i = 0, len = ints.length; i < len; i+=264) {
                        pages.push(ints.slice(i,i+264));
                    }
                    for (var i = 0, len = pages.length; i < len; i+=1) {
                        for (var j = 0, len2 = pages[i].length; j < len2; j+=62) {
                            packets.push(pages[i].slice(j,j+62));
                        }
                    }
                    elm.ports.fromChrome.send({readFile:packets});
                    readingFile = false;
                }
                reader.readAsArrayBuffer(file);
            });
        });
    }
});


elm.ports.toExtension.subscribe(function(message) {
    if (extensionId != null && messageHasValue(message)) {
        chrome.runtime.sendMessage(extensionId, message);
    }
});

deviceSendToElm = function (message) {
    var messageWithNulls = {};
    //if (message.receiveCommand !== undefined)
    //    if (message.receiveCommand[1] !== 0xB9)
    //        console.log("device",message.receiveCommand);
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
