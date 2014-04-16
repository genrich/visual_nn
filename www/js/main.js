function main ()
{
    if (!Detector.webgl)
    {
        document.body.appendChild (Detector.getWebGLErrorMessage ()); 
        return;
    }

    var SIGNALS = window.signals;

    var sig =
    {
        windowResized: new SIGNALS.Signal (),
        windowClosed:  new SIGNALS.Signal (),
        statusUpdated: new SIGNALS.Signal (),
    };

    document.body.appendChild (new Viewport  (sig).dom);
    document.body.appendChild (new Menubar   (sig).dom);
    document.body.appendChild (new Statusbar (sig).dom);
    document.body.appendChild (new Sidebar   (sig).dom);

    window.addEventListener ("resize",       function ()      {sig.windowResized.dispatch ();}, false)
    window.addEventListener ("beforeunload", function (event) {sig.windowClosed. dispatch ();}, false)

    sig.windowResized.dispatch ();
}
