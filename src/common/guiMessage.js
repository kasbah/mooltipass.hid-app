var emptyToGuiMessage = {setLog:[], setConnected:0, setTransferMedia:[0,"",0]};
var emptyFromGuiMessage = {setLog:null, getState:null, startImportMedia: null};

messageHasValue = function (message) {
    var hasValue = false;
    for (var prop in message) {
        hasValue |= message[prop] !== null;
    }
    return hasValue;
}
