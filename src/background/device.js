
var device = {connection: null};
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
    console.log('Found ' + devices.length + ' devices.');
    console.log('Device ' + devices[ind].deviceId + ' vendor' + devices[ind].vendorId + ' product ' + devices[ind].productId);
    var devId = devices[ind].deviceId;

    console.log('Connecting to device '+devId);
    appendToLog('#messageLog', 'Connecting to device...\n');
    chrome.hid.connect(devId, function(connectInfo)
    {
        if (!chrome.runtime.lastError)
		{
            device.connection = connectInfo.connectionId;
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
    device.connection = null;
    chrome.hid.getDevices(device_info, onDeviceFound);
}

