function initParametrization (sig)
{
    var isSimulationRunning = false;
    var params = new function ()
    {
        this.point_size        = 20;
        this.clear_color       = vec3.fromValues (0.7, 0.7, 0.7);
        this.rest_color        = vec3.fromValues (0.2, 0.2, 0.2);
        this.connection_color  = vec3.fromValues (0.5, 0.5, 0.5);
        this.spike_color       = vec3.fromValues (0.9, 0.9, 0);
        this.spike_speed       = CONST.SPIKE_SPEED;
        this.spike_attenuation = 3.0;
        // perspective projection
        this.near          = 1;
        this.far           = 10000;
        this.log_far_const = Math.log (this.far * 0.01  + 1);
        // network
        this.networkRecreate = function ()
        {
            sig.wsSend.dispatch (new Int32Array ([CONST.RECREATE_NETWORK]));
            notifyInfo ('recreating network...');
            isSimulationRunning = false;
            simulationControl.name ('start simulation').updateDisplay ();
        }
        //
        this.stimulus = '';
        // simulation control
        this.simulationToggle = function ()
        {
            if (isSimulationRunning)
            {
                sig.wsSend.dispatch (new Int32Array ([CONST.STOP_SIMULATION]));
                notifyInfo ('stopping simulation...');

                simulationControl.name ('start simulation').updateDisplay ();
            }
            else
            {
                sig.wsSend.dispatch (new Int32Array ([CONST.START_SIMULATION]));
                notifyInfo ('starting simulation...');

                simulationControl.name ('stop simulation').updateDisplay ();
            }
            isSimulationRunning = !isSimulationRunning;
        };
        this.help = function ()
        {
            $.notify ('Navigation with mouse:\nOrbit - left mouse\nMove in/out - middle mouse\nPan - right mouse\nZoom - mousewheel',
                      {className: 'info', autoHideDelay: 15000});
        };
    };

    var gui = new dat.GUI ();
    gui.add (params, 'networkRecreate').name ('recreate network');
    gui.add (params, 'stimulus', ['HelloWorld']);

    var simulationControl = gui.add (params, 'simulationToggle').name ('start simulation');

    gui.add (params, 'spike_attenuation', 0.1, 15).name ('spike attenuation');
    var speedCtrl = gui.add (params, 'spike_speed', 10, 150).name ('spike speed');
    speedCtrl.onFinishChange (function (value)
    {
        var buffer = new ArrayBuffer (8);
        new Uint32Array  (buffer, 0, 1)[0] = CONST.SET_SPIKE_SPEED;
        new Float32Array (buffer, 4, 1)[0] = value;

        sig.wsSend.dispatch (buffer);
    });

    gui.add (params, 'help');

    return params;
}
