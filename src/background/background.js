function launch()
{
    chrome.app.window.create('gui/index.html', {minWidth: 550, minHeight: 600});
}

//chrome.runtime.onInstalled.addListener(launch);
chrome.app.runtime.onLaunched.addListener(launch);

chrome.runtime.onMessageExternal.addListener(function(request, sender, sendResponse)
{
    request.senderId = sender.id;
    console.log('received request '+request.type);

    if (authReq == null) {
        startAuthRequest(request)
    } else {
        authReqQueue.push(request);
    }
});
