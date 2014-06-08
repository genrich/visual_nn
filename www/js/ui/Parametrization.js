function initParametrization (sig)
{
    var isSimulationRunning = false;
    var params = new function ()
    {
        this.clear_color       = vec3.fromValues (0.7, 0.7, 0.7);
        this.rest_color        = vec3.fromValues (0.2, 0.2, 0.2);
        this.spike_color       = vec3.fromValues (0.9, 0.9, 0);
        this.spike_speed       = 100;
        this.spike_attenuation = 3.0;
        // perspective projection
        this.near          = 1;
        this.far           = 10000;
        this.log_far_const = Math.log (this.far * 0.001  + 1);
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
    gui.add (params, 'stimulus', ['HelloWorld']);
    var simulationControl = gui.add (params, 'simulationToggle').name ('start simulation');
    gui.add (params, 'help');

    return params;
}
