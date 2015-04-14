/* This file loads the Elm application and sets up communication with the
   background through chrome.runtime. */

var readingFile = false;
var writingFile = false;

var emptyFromChromeMessage =
    { pickedMediaFile: null
    , readMemFile: null
    };

var gui = Elm.fullscreen(Elm.GUI,
    { fromBackground: emptyToGuiMessage
    , fromChrome: emptyFromChromeMessage
    , fromDevice: []
    }
);


chromeSendToElm = function (message) {
    var messageWithNulls = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyFromChromeMessage) {
        if (message.hasOwnProperty(prop)) {
            messageWithNulls[prop] = message[prop];
        } else {
            messageWithNulls[prop] = emptyFromChromeMessage[prop];
        }
    }
    gui.ports.fromChrome.send(messageWithNulls);
}


chrome.runtime.onMessage.addListener(function(message, sender, sendResponse) {
    if (message.toGUI !== undefined) {
        gui.ports.fromBackground.send(message.toGUI);
    }
    else if (message.fromDevice !== undefined) {
        gui.ports.fromDevice.send(message.fromDevice);
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
                chromeSendToElm({pickedMediaFile:id});
            }
        });
    } else if (message.writeMemFile !== null && ! writingFile) {
        writingFile = true;
        chrome.fileSystem.chooseEntry({type: 'saveFile', suggestedName:"mp-user-data.json"}, (function(data) {
            return function(entry) {
                if (entry != null) {
                    entry.createWriter((function(message) {
                        return function(fileWriter) {
                            fileWriter.onwriteend = function(e) {
                                writingFile = false;
                                chrome.notifications.create(
                                    { type:"basic"
                                    , title:"User data export done."
                                    , message:""
                                    , iconUrl:"/gui/images/logo_square128.png"
                                    });
                            };

                            fileWriter.onerror = function(e) {
                                writingFile = false;
                            };
                            var s = JSON.stringify(data);
                            fileWriter.write(new Blob([s], {type: 'application/json'}));
                        };
                    })(data));
                }
            };
        })(message.writeMemFile));
    } else if (message.readMemFile !== null && ! readingFile) {
        readingFile = true;
        chrome.fileSystem.chooseEntry({type: 'openFile'}, function(entry) {
            if (entry != null) {
                entry.file(function(file) {
                    var reader = new FileReader();
                    reader.onerror = function(e) {
                        console.log(e);
                        readingFile = false;
                    };
                    reader.onloadend = function(e) {
                        var data;
                        try {
                            data = JSON.parse(reader.result);
                        }
                        catch (e) {
                            console.log("invalid file: ", e);
                        }
                        if (data != null) {
                            data.curCardCpz = [];
                            chromeSendToElm({readMemFile:data});
                        }
                        readingFile = false;
                    }
                    reader.readAsText(file);
                });
            } else { readingFile = false;}
        });
    }
});

gui.ports.toDevice.subscribe(function(message) {
    if (message.length > 0)
        chrome.runtime.sendMessage({toDevice: message});
});


