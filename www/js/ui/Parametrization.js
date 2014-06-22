function initParametrization (vnn)
{
    var isSimulationRunning = false;

    vnn.params = new function ()
    {
        this.point_size        = 20;
        this.clear_color       = vec3.fromValues (0.7, 0.7, 0.7);
        this.rest_color        = vec3.fromValues (0.2, 0.2, 0.2);
        this.connection_color  = vec3.fromValues (0.5, 0.5, 0.5);
        this.spike_color       = vec3.fromValues (0.9, 0.9, 0);
        this.spike_speed       = CONST.SPIKE_SPEED;
        this.spike_attenuation = 3.0;

        this.near          = 1;
        this.far           = 10000;
        this.log_far_const = Math.log (this.far * 0.01  + 1);

        this.networkId = 0;

        this.stimulusId = 0;

        this.simulationToggle = function ()
        {
            if (isSimulationRunning)
            {
                vnn.stopSimulation ();
                simulationControl.name ('start simulation').updateDisplay ();
            }
            else
            {
                vnn.startSimulation ();
                simulationControl.name ('stop simulation').updateDisplay ();
            }
            isSimulationRunning = !isSimulationRunning;
        }

        this.networkRecreate = function ()
        {
            if (isSimulationRunning) this.simulationToggle ();

            vnn.createNetwork (this.networkId);
        }

        this.help = function ()
        {
            $.notify ('Navigation with mouse:\nOrbit - left mouse\nMove in/out - middle mouse\nPan - right mouse\nZoom - mousewheel',
                      {className: 'info', autoHideDelay: 15000});
        }
    }

    var gui = new dat.GUI ();
    gui.add (vnn.params, 'networkId', { SixLayers: CONST.NETWORK_0, OneLayer: CONST.NETWORK_1 }).name ('network type');
    gui.add (vnn.params, 'networkRecreate').name ('recreate network');
    gui.add (vnn.params, 'stimulusId', { HelloWorld: CONST.STIMULUS_HELLO_WORLD }).name ('stimulus');

    var simulationControl = gui.add (vnn.params, 'simulationToggle').name ('start simulation');

    gui.add (vnn.params, 'spike_attenuation', 0.1, 15).name ('spike attenuation');
    var speedCtrl = gui.add (vnn.params, 'spike_speed', 10, 150).name ('spike speed');
    speedCtrl.onFinishChange (function (value) { vnn.setSpikeSpeed (value); });

    gui.add (vnn.params, 'help');
}
