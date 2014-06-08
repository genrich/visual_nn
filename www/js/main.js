function notifyInfo    (text) { console.log ('Info: '    + text); $.notify (text, 'info');    }
function notifySuccess (text) { console.log ('Success: ' + text); $.notify (text, 'success'); }
function notifyWarn    (text) { console.log ('Warn: '    + text); $.notify (text, 'warn');    }
function notifyError   (text) { console.log ('Error: '   + text); $.notify (text, 'error');   }

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
    };

    $.notify.defaults ({ globalPosition: 'bottom right' });

    sig.wsOpened.add        (function () { notifySuccess ("Connection to server opened");           });
    sig.wsClosed.add        (function () { notifySuccess ("Connection to server closed");           });
    sig.wsErrorOccurred.add (function () { notifyError   ("Error occurred with server connection"); });

    var params = initParametrization (sig);
    initViewport (sig, params);

    var onWindowResize = function () { sig.windowResized.dispatch (); };
    var onWindowClose  = function () { sig.windowClosed. dispatch (); };
    onWindowResize ();
    window.addEventListener ("resize",       onWindowResize, false);
    window.addEventListener ("beforeunload", onWindowClose,  false);

    var ws = new WebSocket ("ws://localhost:8080/ws_endpoint.yaws");
    ws.binaryType = "arraybuffer";

    sig.wsSend.add  (function (data) { if (ws.readyState == 1) ws.send (data); });
    sig.wsClose.add (function ()     { ws.close ();                            });

    ws.onopen    = function ()     { sig.wsOpened.dispatch        ();          };
    ws.onmessage = function (evnt) { sig.wsMsgReceived.dispatch   (evnt.data); };
    ws.onclose   = function (evnt) { sig.wsClosed.dispatch        (evnt);      };
    ws.onerror   = function ()     { sig.wsErrorOccurred.dispatch ();          };
}
