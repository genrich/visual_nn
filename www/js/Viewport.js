function Viewport (sig)
{
    var mvMatrix = mat4.create (),
        pMatrix  = mat4.create ();

    function spike (time)
    {
        if (Math.random () < 0.3)
            nodes.randomSpike (time);
    }

    function draw (timestamp)
    {
        var time = timestamp / 1000.0;

        spike (time);

        gl.clear (gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        nodes.drawNodes       (pMatrix, mvMatrix);
        nodes.drawConnections (pMatrix, mvMatrix);
        nodes.drawSpikes      (pMatrix, mvMatrix, time);

        requestAnimationFrame (draw);
    }

    sig.windowResized.add (function ()
    {
        canvas.width  = viewport.dom.clientWidth;
        canvas.height = viewport.dom.clientHeight;

        mat4.perspective (pMatrix, Math.PI / 4, canvas.width / canvas.height, 1, 10000);

        gl.viewport (0, 0, canvas.width, canvas.height);
    })

    var viewport = new UI.Panel ({clazz: 'Viewport'});

    if (!Detector.webgl)
        return Detector.addGetWebGLMessage ({ parent: viewport });

    var canvas = document.createElement ('canvas');
    var gl = canvas.getContext ('webgl');
    viewport.dom.appendChild (canvas);

    var controller = new MouseController (canvas, mvMatrix);

    var nodes = new Nodes (gl);

    // nodes.set (0, -100, -100, 0); nodes.set (1, -100,  100, 0);
    // nodes.set (2,  100, -100, 0); nodes.set (3,  100,  100, 0); nodes.set (4, 0, 0, 100);
    // nodes.connect (0, 4); nodes.connect (1, 4); nodes.connect (2, 4); nodes.connect (3, 4);

    for (var i = 0; i < 100; ++i)
        nodes.set (i, Math.random () * 500 - 250, Math.random () * 1000 - 500, Math.random () * 500 - 250);
    for (var i = 0; i < 100; ++i)
        nodes.connect (nodes.randomNode (), nodes.randomNode ());

    gl.clearColor (0.9, 0.9, 0.9, 1.0);
    gl.enable (gl.DEPTH_TEST);

    mat4.identity    (mvMatrix);
    mat4.translate   (mvMatrix, mvMatrix, [0, 0, -1500]);

    requestAnimationFrame (draw);

    return viewport;
};
