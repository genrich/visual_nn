var Statusbar = function (sig)
{
	var statusbar = new UI.Panel ()
	statusbar.setClass ("Statusbar")

    sig.statusUpdated.add (function (text) { statusbar.setTextContent (text) })

    var log = function (text)
    {
        statusbar.setTextContent (text)
        console.log (text)
    }

    sig.wsOpened.add        (function ()  { log ("Connection to server opened")           })
    sig.wsClosed.add        (function ()  { log ("Connection to server closed")           })
    sig.wsErrorOccurred.add (function ()  { log ("Error occurred with server connection") })

	return statusbar
}
