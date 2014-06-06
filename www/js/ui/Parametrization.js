function initParametrization (sig)
{
    var isSimulationRunning = false;
    var params =
    {
        stimulus: '',
        simulationToggle: function ()
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
        },
        help: function ()
        {
            $.notify ('Navigation with mouse:\nOrbit - left mouse\nMove in/out - middle mouse\nPan - right mouse\nZoom - mousewheel',
                      {className: 'info', autoHideDelay: 15000});
        },
    };

    var gui = new dat.GUI ();
    gui.add (params, 'stimulus', ['HelloWorld']);
    var simulationControl = gui.add (params, 'simulationToggle').name ('start simulation');
    gui.add (params, 'help');
}
