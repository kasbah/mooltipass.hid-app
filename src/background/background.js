function launch()
{
    chrome.app.window.create('gui/index.html', {minWidth: 550, minHeight: 600});
}

//chrome.runtime.onInstalled.addListener(launch);
chrome.app.runtime.onLaunched.addListener(launch);
