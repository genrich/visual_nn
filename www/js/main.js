function notifyInfo    (text) { console.log ('Info: '    + text); $.notify (text, 'info');    }
function notifySuccess (text) { console.log ('Success: ' + text); $.notify (text, 'success'); }
function notifyWarn    (text) { console.log ('Warn: '    + text); $.notify (text, 'warn');    }
function notifyError   (text) { console.log ('Error: '   + text); $.notify (text, 'error');   }

function main ()
{
    $.notify.defaults ({ globalPosition: 'bottom right' });

    var ws;
    function send (data)
    {
        if (ws.readyState == 1)
            ws.send (data);
    }

    var vnn = new function ()
    {
        this.windowResized    = new signals.Signal ();
        this.windowClosed     = new signals.Signal ();
        this.connectionOpened = new signals.Signal ();
        this.connectionClosed = new signals.Signal ();
        this.connectionError  = new signals.Signal ();
        this.messageReceived  = new signals.Signal ();

        this.connect = function ()
        {
            ws = new WebSocket ("ws://localhost:8080/ws_endpoint.yaws");
            ws.binaryType = "arraybuffer";
            ws.onopen     = function ()     { vnn.connectionOpened.dispatch ();          };
            ws.onmessage  = function (evnt) { vnn.messageReceived. dispatch (evnt.data); };
            ws.onclose    = function (evnt) { vnn.connectionClosed.dispatch (evnt);      };
            ws.onerror    = function ()     { vnn.connectionError. dispatch ();          };
        }

        this.disconnect = function ()
        {
            ws.close ();
        }

        this.createNetwork = function (networkId)
        {
            notifyInfo ('Creating network...');
            send (new Int32Array ([CONST.RECREATE_NETWORK, networkId]));
        }

        this.startSimulation = function ()
        {
            notifyInfo ('Starting simulation...');
            send (new Uint32Array ([CONST.START_SIMULATION]));
        }

        this.stopSimulation = function ()
        {
            notifyInfo ('Stopping simulation...');
            send (new Uint32Array ([CONST.STOP_SIMULATION]));
        }

        this.setSpikeSpeed = function (speed)
        {
            var buffer = new ArrayBuffer (8);
            new Uint32Array  (buffer, 0, 1)[0] = CONST.SET_SPIKE_SPEED;
            new Float32Array (buffer, 4, 1)[0] = speed;

            send (buffer);
        }
    };

    vnn.connectionOpened.add (function ()
    {
        notifySuccess ('Connection to server opened');
        vnn.createNetwork (CONST.NETWORK_0);
    });

    vnn.connectionClosed.add (function ()
    {
        notifySuccess ('Connection to server closed');
    });

    vnn.connectionError.add (function ()
    {
        notifyError ('Error occurred with server connection');
    });

    initParametrization (vnn);
    initViewport (vnn);

    var onWindowResize = function () { vnn.windowResized.dispatch (); };
    var onWindowClose  = function () { vnn.windowClosed. dispatch (); };
    onWindowResize ();
    addEventListener ("resize",       onWindowResize, false);
    addEventListener ("beforeunload", onWindowClose,  false);

    vnn.connect ();
}
