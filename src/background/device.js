var device = {connection: null, connecting: 0, waitingForStatus : false};
var device_info = { "vendorId": 0x16d0, "productId": 0x09a0 };      // Mooltipass
var PACKET_SIZE = 64;

var cmd_EXPORT_FLASH_START  = 0x8A;
var cmd_EXPORT_FLASH        = 0x8B;
var cmd_EXPORT_FLASH_END    = 0x8C;
var cmd_IMPORT_FLASH_BEGIN  = 0x8D;
var cmd_IMPORT_FLASH        = 0x8E;
var cmd_IMPORT_FLASH_END    = 0x8F;
var cmd_EXPORT_EEPROM_START = 0x90;
var cmd_EXPORT_EEPROM       = 0x91;
var cmd_EXPORT_EEPROM_END   = 0x92;
var cmd_IMPORT_EEPROM_BEGIN = 0x93;
var cmd_IMPORT_EEPROM       = 0x93;
var cmd_IMPORT_EEPROM_END   = 0x94;
var cmd_ERASE_EEPROM        = 0x95;
var cmd_ERASE_FLASH         = 0x96;
var cmd_ERASE_SMC           = 0x97;
var cmd_DRAW_BITMAP         = 0x98;
var cmd_SET_FONT            = 0x99;
var cmd_USB_KEYBOARD_PRESS  = 0x9A;
var cmd_STACK_FREE          = 0x9B;
var cmd_CLONE_SMARTCARD     = 0x9C;
var cmd_DEBUG               = 0xA0;
var cmd_PING                = 0xA1;
var cmd_VERSION             = 0xA2;
var cmd_CONTEXT             = 0xA3;
var cmd_GET_LOGIN           = 0xA4;
var cmd_GET_PASSWORD        = 0xA5;
var cmd_SET_LOGIN           = 0xA6;
var cmd_SET_PASSWORD        = 0xA7;
var cmd_CHECK_PASSWORD      = 0xA8;
var cmd_ADD_CONTEXT         = 0xA9;
var cmd_SET_BOOTLOADER_PWD  = 0xAA;
var cmd_JUMP_TO_BOOTLOADER  = 0xAB;
var cmd_GET_RANDOM_NUMBER   = 0xAC;
var cmd_START_MEMORYMGMT    = 0xAD;
var cmd_IMPORT_MEDIA_START  = 0xAE;
var cmd_IMPORT_MEDIA        = 0xAF;
var cmd_IMPORT_MEDIA_END    = 0xB0;
var cmd_SET_MOOLTIPASS_PARM = 0xB1;
var cmd_GET_MOOLTIPASS_PARM = 0xB2;
var cmd_RESET_CARD          = 0xB3;
var cmd_READ_CARD_LOGIN     = 0xB4;
var cmd_READ_CARD_PASS      = 0xB5;
var cmd_SET_CARD_LOGIN      = 0xB6;
var cmd_SET_CARD_PASS       = 0xB7;
var cmd_ADD_UNKNOWN_CARD    = 0xB8;
var cmd_MOOLTIPASS_STATUS   = 0xB9;
var cmd_FUNCTIONAL_TEST_RES = 0xBA;
var cmd_SET_DATE            = 0xBB;
var cmd_SET_UID             = 0xBC;
var cmd_GET_UID             = 0xBD;
var cmd_SET_DATA_SERVICE    = 0xBE;
var cmd_ADD_DATA_SERVICE    = 0xBF;
var cmd_WRITE_32B_IN_DN     = 0xC0;
var cmd_READ_32B_IN_DN      = 0xC1;
var cmd_GET_CUR_CARD_CPZ    = 0xC2;
var cmd_READ_FLASH_NODE     = 0xC5;
var cmd_WRITE_FLASH_NODE    = 0xC6;
var cmd_GET_FAVORITE        = 0xC7;
var cmd_SET_FAVORITE        = 0xC8;
var cmd_GET_STARTING_PARENT = 0xC9;
var cmd_SET_STARTING_PARENT = 0xCA;
var cmd_GET_CTRVALUE        = 0xCB;
var cmd_SET_CTRVALUE        = 0xCC;
var cmd_ADD_CARD_CPZ_CTR    = 0xCD;
var cmd_GET_CARD_CPZ_CTR    = 0xCE;
var cmd_CARD_CPZ_CTR_PACKET = 0xCF;
var cmd_GET_FREE_SLOTS      = 0xD0;
var cmd_GET_DN_START_PARENT = 0xD1;
var cmd_SET_DN_START_PARENT = 0xD2;
var cmd_END_MEMORYMGMT      = 0xD3;

/**
 * Handler invoked when new USB mooltipass devices are found.
 * @param devices array of device objects
 * @note only the last device is used, assumes that one mooltipass is present.
 * Stale entries appear to be left in chrome if the mooltipass is removed
 * and plugged in again, or the firmware is updated.
 */
onDeviceFound = function (devices)
{
    if (devices.length <= 0)
    {
        return;
    }

    var ind = devices.length - 1;
    var devId = devices[ind].deviceId;

    chrome.hid.connect(devId, function(connectInfo)
    {
        if (!chrome.runtime.lastError)
		{
            device.connection = connectInfo.connectionId;
            deviceSendToElm({setHidConnected:true});
            //make sure then next status won't be dropped because of dropRepeats
            elm.ports.deviceStatus.send(7);
            deviceSendToElm({appendToLog:"device found, connection made"});
        }
        clearTimeout(device.timeoutId);
        device.connecting = false;
    });
}

device.connect = function ()
{
    if (device.connecting === 1)
        return;
    else if (device.connecting === 0) {
        deviceSendToElm({appendToLog:"> looking for device"});
    }
    device.connecting = 1;
    device.timeoutId = setTimeout(function () {
        if (device.connecting === 1) {
            device.connecting = 2;
        }
    }, 5000)
    chrome.hid.getDevices(device_info, onDeviceFound);
}


function onDataReceived(reportId, data)
{
    var bytes = new Uint8Array(data);
    var ints = [];
    for (var i = 0, len = bytes.length; i < len; i++)
    {
        ints[i] = bytes[i];
    }
    if (ints[1] === cmd_MOOLTIPASS_STATUS) {
        //console.log("<<");
        elm.ports.deviceStatus.send(ints[2]);
        device.waitingForStatus = false;
    }
    //route some messages to GUI only
    else if (ints[1] === cmd_ADD_UNKNOWN_CARD) {
        chrome.runtime.sendMessage({fromDevice:ints});
    } else  {
        if (ints[1] == cmd_GET_CUR_CARD_CPZ) // route to both
            chrome.runtime.sendMessage({fromDevice:ints});
        deviceSendToElm({receiveCommand: ints});
    }

    //special case for 'read node' and 'cpz ctr packet export' messages as we
    //need to read multiple messages in a row a row
    if (ints[1] === cmd_READ_FLASH_NODE || ints[1] === cmd_CARD_CPZ_CTR_PACKET)
        chrome.hid.receive(device.connection, onDataReceived);
}

function hidErrorDisconnect(message) {
        console.log("hid error: ", message);
        device.connecting = 0;
        device.connection = null;
        device.waitingForStatus = false;
        deviceSendToElm({setHidConnected:false});
        //make sure then next status won't be dropped because of dropRepeats
        elm.ports.deviceStatus.send(7);
}

function sendMsg(message)
{
    if (device.connection == null) {
        hidErrorDisconnect("no connection when trying to send message")
        return;
    }
    if (message[1] === 0xB9) { //status update
        if (device.waitingForStatus)
            return;
        else
            device.waitingForStatus = true;
    }
    //else
    //    console.log("app", message);
    //Buffer creation is a bit awkward because Windows doesn't like us using
    //the Uint8Array.buffer directly (or maybe it's something to do with the
    //ArrayBuffer size argument?). This is what works on all platforms equally.
    var buffer = new ArrayBuffer(PACKET_SIZE);
    var view = new Uint8Array(buffer);
    view.set(message,0);
    chrome.hid.send(device.connection, 0, buffer, function()
    {
        if (!chrome.runtime.lastError)
        {
            chrome.hid.receive(device.connection, onDataReceived);
        }
        else
        {
            hidErrorDisconnect(chrome.runtime.lastError.message)
        }
    });
}
