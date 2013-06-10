var Menubar = function (sig)
{
	var menubar = new UI.Panel ()
	menubar.setClass ("Menubar")

    menubar.add (function ()
    {
        var stimulusMenu = new UI.Panel ({clazz: "menu"})
        stimulusMenu.add (new UI.Panel ({clazz: "title", text: "Stimulus"}))
        var options = new UI.Panel ({clazz: "options"})
        stimulusMenu.add (options)
        var helloWorldOption = new UI.Panel ({clazz: "option", text: String.fromCharCode (10004) + " Hello World"})
        options.add (helloWorldOption)
        return stimulusMenu
    } ())

    menubar.add (function ()
    {
        var simulationMenu = new UI.Panel ({ clazz: "menu" })
        simulationMenu.add (new UI.Panel ({ clazz: "title", text: "Simulation" }))
        var options = new UI.Panel ({ clazz: "options" })
        simulationMenu.add (options)

        var simStartOption = new UI.Panel ({ clazz: "option", text: "Start" })
        var simStatus = 0
        simStartOption.onClick (function  ()
        {
            if (simStatus == 0 && ws.readyState == 1)
            {
                simStatus = 1
                ws.send (new Int32Array ([simStatus]))

                simStartOption.setClass ("option_disabled")
                simStopOption.setClass ("option")
                sig.statusUpdated.dispatch ("Simulation started")
            }
        })
        options.add (simStartOption)

        var simStopOption = new UI.Panel ({ clazz: "option_disabled", text: "Stop" })
        simStopOption.onClick (function  ()
        {
            if (simStatus == 1)
            {
                simStatus = 0
                ws.send (new Int32Array ([simStatus]))

                simStartOption.setClass ("option")
                simStopOption.setClass ("option_disabled")
                sig.statusUpdated.dispatch ("Simulation stopped")
            }
        })
        options.add (simStopOption)

        sig.wsOpened.add (function () { simStartOption.setClass ("option") })
        sig.wsClosed.add (function () { simStartOption.setClass ("option_disabled")
                                        simStopOption. setClass ("option_disabled")})

        return simulationMenu
    } ())


	menubar.add (function ()
    {
        var helpMenu = new UI.Panel ({ clazz: "menu" })

        helpMenu.add (new UI.Panel ({ clazz: "title", text: "Help" }))

        var options = new UI.Panel ({ clazz: "options" })
        helpMenu.add (options)

        var option = new UI.Panel ({ clazz: "option", text: "About" })
        option.onClick (function  () { sig.statusUpdated.dispatch ("About Click") })
        options.add (option)

        return helpMenu
    } ())

    return menubar
}
