/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var emptyDeviceMessage = {setConnected: null};
var elm = Elm.worker(Elm.Background, {fromGUI: emptyFromGuiMessage, fromDevice: emptyDeviceMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toBackground != null) {
            elm.ports.fromGUI.send(request.toBackground);
        }
    });

elm.ports.toGUI.subscribe(function(message) {
        chrome.runtime.sendMessage({toGUI: message});
    });

elm.ports.toDevice.subscribe(function(connected) {
    if (connected) {
        return;
    } else {
        connect();
    }
});

sendToElm = function (obj) {
    var msg = {};
    //replace undefined with null so it becomes 'Nothing' in Elm
    for (var prop in emptyDeviceMessage) {
        if(obj.hasOwnProperty(prop)){
            msg[prop] = obj[prop];
        } else {
            msg[prop] = emptyDeviceMessage[prop];
        }
    }
    elm.ports.fromDevice.send(msg);
};

function launch()
{
    chrome.app.window.create('gui/index.html', {minWidth: 550, minHeight: 600});
}

//chrome.runtime.onInstalled.addListener(launch);
chrome.app.runtime.onLaunched.addListener(launch);

//chrome.runtime.onMessageExternal.addListener(function(request, sender, sendResponse)
//{
//    request.senderId = sender.id;
//    console.log('received request '+request.type);
//
//    if (authReq == null) {
//        startAuthRequest(request)
//    } else {
//        authReqQueue.push(request);
//    }
//});
