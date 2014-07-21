function initParametrization (vnn)
{
    var isSimulationRunning = false;

    vnn.params = new function ()
    {
        this.near          = 1;
        this.far           = 10000;
        this.log_far_const = Math.log (this.far * 0.01  + 1);

        this.point_size = 20;

        this.clear_color_hex      = '#b3b3b3';
        this.rest_color_hex       = '#323232';
        this.connection_color_hex = '#7f7f7f';
        this.spike_color_hex      = '#ffff00';
        this.hover_color_hex      = '#e60000';
        this.selected_color_hex   = '#b30000';
        this.inbound_color_hex    = '#007700';
        this.outbound_color_hex   = '#000077';
        this.neighbour_color_hex  = '#640000';

        this.clear_color      = toColor (this.clear_color_hex);
        this.rest_color       = toColor (this.rest_color_hex);
        this.connection_color = toColor (this.connection_color_hex);
        this.spike_color      = toColor (this.spike_color_hex);
        this.hover_color      = toColor (this.hover_color_hex);
        this.selected_color   = toColor (this.selected_color_hex);
        this.inbound_color    = toColor (this.inbound_color_hex);
        this.outbound_color   = toColor (this.outbound_color_hex);
        this.neighbour_color  = toColor (this.neighbour_color_hex);

        this.slowdown            = CONST.PARAM_SLOWDOWN;
        this.spike_speed         = CONST.PARAM_SPIKE_SPEED;
        this.absolute_refractory = CONST.PARAM_ABSOLUTE_REFRACTORY;

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
            $.notify ('Navigation with mouse:\nOrbit - left mouse\nMove in/out - middle mouse\nPan - right mouse\nZoom - mousewheel'
                      + '\nClick - inbound/outbound/neighbours',
                      {className: 'info', autoHideDelay: 15000});
        }
    }

    var gui = new dat.GUI ();

    var colors = gui.addFolder ('colors');
    addColorToFolder (colors, 'clear_color',      'clear');
    addColorToFolder (colors, 'rest_color',       'rest');
    addColorToFolder (colors, 'connection_color', 'connection');
    addColorToFolder (colors, 'spike_color',      'spike');
    addColorToFolder (colors, 'hover_color',      'hover');

    var selection_colors = colors.addFolder ('selection');
    addColorToFolder (selection_colors, 'selected_color',  'selected');
    addColorToFolder (selection_colors, 'inbound_color',   'inbound');
    addColorToFolder (selection_colors, 'outbound_color',  'outbound');
    addColorToFolder (selection_colors, 'neighbour_color', 'neighbour');

    gui.add (vnn.params, 'networkId',
    {
        SixLayers: CONST.NETWORK_0,
        OneLayer:  CONST.NETWORK_1,
        Test:      CONST.NETWORK_2,
    }).name ('network type');
    gui.add (vnn.params, 'networkRecreate').name ('recreate network');
    gui.add (vnn.params, 'stimulusId', { HelloWorld: CONST.STIMULUS_HELLO_WORLD }).name ('stimulus');

    var simulationControl = gui.add (vnn.params, 'simulationToggle').name ('start simulation');

    gui.add (vnn.params, 'slowdown', vnn.params.slowdown / 10, vnn.params.slowdown * 10)
        .onFinishChange (function (value) { vnn.setSlowdown (value); });

    gui.add (vnn.params, 'help');

    function toColor (hex)
    {
        var v = hex.match (/^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i);
        return vec3.fromValues (parseInt (v[1], 16) / 255.0, parseInt (v[2], 16) / 255.0, parseInt (v[3], 16) / 255.0);
    }

    function addColorToFolder (folder, param, name)
    {
        folder.addColor (vnn.params, param + '_hex').name (name).onChange (function (rgb) { vnn.params[param] = toColor (rgb); });
    }
}
