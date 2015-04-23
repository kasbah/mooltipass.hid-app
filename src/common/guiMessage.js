var emptyToGuiMessage =
    { setLog          : []
    , setDeviceStatus : 0
    , setImportInfo   : [0,"",0,0]
    , setMemInfo      : [0,null]
    , setParameter    : null
    , getParameter    : null
    , settingsInfo    : { keyboard: 0x93, timeout: 3 , offline: false, screensaver: true}
    };

var emptyFromGuiMessage =
    { setLog           : null
    , getState         : null
    , startImportMedia : null
    , startMemManage   : null
    , endMemManage     : null
    , saveMemManage    : null
    , setParameter     : null
    , getParameter     : null
    };

messageHasValue = function (message) {
    var hasValue = false;
    for (var prop in message) {
        hasValue |= message[prop] !== null;
    }
    return hasValue;
}
