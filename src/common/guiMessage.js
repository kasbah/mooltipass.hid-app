var emptyToGuiMessage =
    { setLog:[]
    , setConnected:0
    , setTransferInfo:[0,"",0,0]
    , setMemoryInfo: { favorites: [], credentials: []}
};

var emptyFromGuiMessage =
    { setLog           : null
    , startImportMedia : null
    };

messageHasValue = function (message) {
    var hasValue = false;
    for (var prop in message) {
        hasValue |= message[prop] !== null;
    }
    return hasValue;
}
