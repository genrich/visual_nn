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
        }
    };

    var gui = new dat.GUI ();
    gui.add (params, 'stimulus', ['HelloWorld']);
    var simulationControl = gui.add (params, 'simulationToggle').name ('start simulation');
}
