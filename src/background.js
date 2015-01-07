function launch()
{
    chrome.app.window.create('index.html', {minWidth: 550, minHeight: 600});
}

chrome.runtime.onInstalled.addListener(launch);
chrome.runtime.onStartup.addListener(launch);
