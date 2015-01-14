/* This file loads the Elm application and sets up communication with the
   gui through chrome.runtime. */
var connection = null;
var emptyMpMessage = {appendToLog: null, setConnected: null};
var elm = Elm.worker(Elm.Background, {fromGUI: emptyGuiMessage, fromMP: emptyMpMessage});

chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
        if (request.toBackground != null) {
            elm.ports.fromGUI.send(request.toBackground);
        }
    });

elm.ports.toGUI.subscribe(function(message) {
        chrome.runtime.sendMessage({toGUI: message});
    });

elm.ports.toDevice.subscribe(function(ints) {
    var buffer = new Uint8Array(ints)
    chrome.hid.send(connection, 0, buffer, function()
    {
        if (!chrome.runtime.lastError)
        {
            chrome.hid.receive(device.connection, onDataReceived);
        }
        else
        {
            console.log('Failed to send to device: '+chrome.runtime.lastError.message);
        }
    });
}

function sendToElm (obj) {
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

function appendToLog(logId, text)
{
    console.log(logId, text);
    sendToElm({appendToLog:logId + ":" + text});
}

/**
 * Handler invoked when new USB mooltipass devices are found.
 * Connects to the device and sends a version request.
 * @param devices array of device objects
 * @note only the last device is used, assumes that one mooltipass is present.
 * Stale entries appear to be left in chrome if the mooltipass is removed
 * and plugged in again, or the firmware is updated.
 */
function onDeviceFound(devices)
{
    if (devices.length <= 0)
    {
        return;
    }

    var ind = devices.length - 1;
    console.log('Found ' + devices.length + ' devices.');
    console.log('Device ' + devices[ind].deviceId + ' vendor' + devices[ind].vendorId + ' product ' + devices[ind].productId);
    var devId = devices[ind].deviceId;

    console.log('Connecting to device '+devId);
    appendToLog('#messageLog', 'Connecting to device...\n');
    chrome.hid.connect(devId, function(connectInfo)
    {
        if (!chrome.runtime.lastError)
		{
            connection = connectInfo.connectionId;

        }
        else
        {
          console.log('Failed to connect to device: '+chrome.runtime.lastError.message);
        }
    });
}

/**
 * Connect to the mooltipass
 */
function connect(msg)
{
    connection = null;
    chrome.hid.getDevices(device_info, onDeviceFound);
}

