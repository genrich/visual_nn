function initViewport (sig, params)
{
    var viewport = $('#viewport')[0];
    var canvas   = $('#canvas')  [0];

    var gl = canvas.getContext ('webgl', { alpha: false });

    gl.clearColor (params.clear_color[0], params.clear_color[1], params.clear_color[2], 1.0);

    gl.enable (gl.DEPTH_TEST);
    gl.enable (gl.BLEND);
    gl.blendFunc (gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    var mvMatrix = mat4.create (), pMatrix = mat4.create ();

    initController (canvas, mvMatrix);

    var network = new Network (gl, params);

    requestAnimationFrame (draw);

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
        else if (type == CONST.NEW_NETWORK)
        {
            network.clear ();
        }
    })

    sig.windowResized.add (function ()
    {
        canvas.width  = viewport.clientWidth;
        canvas.height = viewport.clientHeight;

        mat4.perspective (pMatrix, Math.PI / 4, canvas.width / canvas.height, params.near, params.far);
        gl.viewport (0, 0, canvas.width, canvas.height);
    });

    if (webGLIsNotSupported ())
    {
        $('#viewport').html ("<div style='height:100%; width:100%; display:table;'>"
                + "<div style='display:table-cell; vertical-align:middle; text-align:center;'>"
                + "Your browser doesn't support <a href='http://caniuse.com/webgl'>WebGL</a></div></div>");
        throw 'WebGL not supported!';
    }
}
