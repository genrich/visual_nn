function initViewport (sig)
{
    function draw (timestamp)
    {
        var time = timestamp / 1000.0;

        gl.clear (gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        network.drawNodes       (pMatrix, mvMatrix, time);
        network.drawConnections (pMatrix, mvMatrix);
        network.drawSpikes      (pMatrix, mvMatrix, time);

        requestAnimationFrame (draw);
    }

    function webGLIsNotSupported ()
    {
        try       { return !window.WebGLRenderingContext || !canvas.getContext ('webgl'); }
        catch (e) { return true; }
    };

    sig.wsMsgReceived.add (function (buffer)
    {
        var type = new Uint32Array (buffer, 0, 1)[0];

        if (type == CONST.SPIKE)
        {
            var id = new Uint32Array (buffer, 4, 1)[0];
            network.spike (id);
        }
        else if (type == CONST.POSITION)
        {
            var id = new Uint32Array (buffer, 4, 1)[0];
            var pos = new Float32Array (buffer, 8,  3);
            network.set (id, pos[0], pos[1], pos[2]);
        }
        else if (type == CONST.CONNECTION)
        {
            var ids = new Uint32Array (buffer, 4, 2);
            network.connect (ids[0], ids[1]);
        }
    })

    var viewport = $('#viewport')[0];
    var canvas   = $('#canvas')  [0];

	if (webGLIsNotSupported ())
    {
        $('#viewport').html ("<div style='height:100%; width:100%; display:table;'>"
                + "<div style='display:table-cell; vertical-align:middle; text-align:center;'>"
                + "Your browser doesn't support <a href='http://caniuse.com/webgl'>WebGL</a></div></div>");
        throw 'WebGL not supported!';
    }

    var gl = canvas.getContext ('webgl');

    gl.clearColor (0.7, 0.7, 0.7, 1.0);
    gl.enable (gl.DEPTH_TEST);

    var mvMatrix = mat4.create (), pMatrix = mat4.create ();
    mat4.identity  (mvMatrix);
    mat4.translate (mvMatrix, mvMatrix, [0, 0, -1500]);

    sig.windowResized.add (function ()
    {
        canvas.width  = viewport.clientWidth;
        canvas.height = viewport.clientHeight;

        mat4.perspective (pMatrix, Math.PI / 4, canvas.width / canvas.height, 1, 10000);
        gl.viewport (0, 0, canvas.width, canvas.height);
    });

    initController (canvas, mvMatrix);

    var network = new Network (gl);

    requestAnimationFrame (draw);
}
