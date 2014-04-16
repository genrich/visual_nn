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

	return statusbar
}
