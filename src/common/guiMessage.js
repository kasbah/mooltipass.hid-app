var emptyToGuiMessage = {setLog:[], setConnected:0, setTransferInfo:[0,"",0,0]};
var emptyFromGuiMessage =
    { setLog           : null
    , getState         : null
    , startImportMedia : null
    , addToFavs        : null
    , removeFromFavs   : null
    };

messageHasValue = function (message) {
    var hasValue = false;
    for (var prop in message) {
        hasValue |= message[prop] !== null;
    }
    return hasValue;
}
