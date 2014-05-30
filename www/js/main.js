var ws = new WebSocket ("ws://localhost:8080/ws_endpoint.yaws")

function main ()
{
    var SIGNALS = window.signals;

    var sig =
    {
        windowResized:   new SIGNALS.Signal (),
        windowClosed:    new SIGNALS.Signal (),
        wsSend:          new SIGNALS.Signal (),
        wsClose:         new SIGNALS.Signal (),
        wsOpened:        new SIGNALS.Signal (),
        wsMsgReceived:   new SIGNALS.Signal (),
        wsClosed:        new SIGNALS.Signal (),
        wsErrorOccurred: new SIGNALS.Signal (),
        statusUpdated:   new SIGNALS.Signal (),
    }

    document.body.appendChild (new Viewport  (sig).dom)
    document.body.appendChild (new Menubar   (sig).dom)
    document.body.appendChild (new Statusbar (sig).dom)
    document.body.appendChild (new Sidebar   (sig).dom)

    var onWindowResize = function (event) { sig.windowResized.dispatch () }
    var onWindowClose  = function (event) { sig.windowClosed. dispatch () }

    onWindowResize ()

    window.addEventListener ("resize",       onWindowResize, false)
    window.addEventListener ("beforeunload", onWindowClose,  false)

    ws.binaryType = "arraybuffer"

    sig.wsSend.add  (function (data) { if (ws.readyState == 1) ws.send (data) })
    sig.wsClose.add (function ()     { ws.close ()                            })

    ws.onopen    = function ()     { sig.wsOpened.dispatch        ()          }
    ws.onmessage = function (evnt) { sig.wsMsgReceived.dispatch   (evnt.data) }
    ws.onclose   = function (evnt) { sig.wsClosed.dispatch        (evnt)      }
    ws.onerror   = function ()     { sig.wsErrorOccurred.dispatch ()          }
}
