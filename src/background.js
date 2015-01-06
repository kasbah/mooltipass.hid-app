function launch()
{
    chrome.app.window.create('index.html', {minWidth: 550, minHeight: 600});

    chrome.runtime.onMessage.addListener(function(request, sender, sendResponse)
    {
        console.log(sender.tab ?  'from a content script:' + sender.tab.url : 'from the extension');
        if (request.type == 'inputs')
        {
            console.log('inputs:');
            for (var i=0; i<request.inputs.length; i++)
            {
                console.log('    "'+request.inputs[i].id);
            }
            sendResponse({type: 'response', msg: 'Hi From Client'});
        }
    });
}

chrome.runtime.onInstalled.addListener(launch);
chrome.runtime.onStartup.addListener(launch);
