var device = {connection: null, connecting: false};
var device_info = {vendorId: 0x16d0, productId: 0x09a0};

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
    var devId = devices[ind].deviceId;
    console.log("devId", devId);

    chrome.hid.connect(devId, function(connectInfo)
    {
        console.log("connectInfo", connectInfo);
        if (!chrome.runtime.lastError)
		{
            device.connection = connectInfo.connectionId;
        }
        console.log(device.connection)
        if (device.connection !== null) {
            sendToElm({setConnected:"Connected"});
        } else {
            sendToElm({setConnected:"NotConnected"});
        }
        device.connecting = false;
        clearTimeout(this.timeoutId);
    });
}

/**
 * Connect to the mooltipass
 */
device.checkConnection = function()
{
    if (device.connecting) {
        return;
    }
    device.connecting = true;
    device.connection = null;
    sendToElm({appendToLog:"> connecting to device"});
    this.timeoutId = setTimeout(function () {
        if (device.connecting) {
            sendToElm({appendToLog:"connection attempt timed out"});
            device.connecting = false;
        }
    }, 5000)
    chrome.hid.getDevices({}, onDeviceFound.bind());
}

