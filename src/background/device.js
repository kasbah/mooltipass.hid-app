var device = {connection: null, connecting: false};

/**
 * Handler invoked when new USB mooltipass devices are found.
 * Connects to the device and sends a version request.
 * @param devices array of device objects
 * @note only the last device is used, assumes that one mooltipass is present.
 * Stale entries appear to be left in chrome if the mooltipass is removed
 * and plugged in again, or the firmware is updated.
 */
device.onDeviceFound = function (devices)
{
    if (devices.length <= 0)
    {
        return;
    }

    var ind = devices.length - 1;
    var devId = devices[ind].deviceId;

    console.log("onDeviceFound", devId);
    chrome.hid.connect(devId, function(connectInfo)
    {
        console.log(connectInfo);
        if (!chrome.runtime.lastError)
		{
            device.connection = connectInfo.connectionId;
        }
        if (device.connection !== null) {
            sendToElm({setConnected:"Connected"});
        } else {
            sendToElm({setConnected:"NotConnected"});
        }
        device.connecting = false;
        clearTimeout(device.timeoutId);
    });
}

/**
 * Connect to the mooltipass
 */
device.checkConnection = function()
{
    if (!device.connecting) {
        if (device.connection !== null) {
            sendMsg((new Uint8Array([2,0x04,0x02])).buffer)
        }
        if (device.connection === null) {
            device.connect()
        }
    }
}

device.connect = function ()
{
    sendToElm({appendToLog:"> connecting to device"});
    device.connecting = true;
    device.timeoutId = setTimeout(function () {
        if (device.connecting) {
            sendToElm({appendToLog:"connection attempt timed out"});
            device.connecting = false;
        }
    }, 5000)
    chrome.hid.getDevices({}, device.onDeviceFound);
}


function onDataReceived(reportId, data)
{
    var bytes = new Uint8Array(data);
    var msg = new Uint8Array(data,2);
    var len = bytes[0]
    var cmd = bytes[1]

    console.log('Received CMD ' + cmd + ', len ' + len);
    console.log(msg);
}

function sendMsg(msg)
{
    chrome.hid.send(device.connection, 0, msg, function()
    {
        if (!chrome.runtime.lastError)
        {
            chrome.hid.receive(device.connection, onDataReceived);
        }
        else
        {
            console.log("hid error", chrome.runtime.lastError);
        }
    });
}
