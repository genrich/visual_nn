function initViewport (vnn)
{
    const FRAMEBUFFER_SIZE = 512;

    var viewport = $('#viewport')[0];
    var canvas   = $('#canvas')  [0];

    var gl = canvas.getContext ('webgl', { alpha: false });

    gl.enable (gl.DEPTH_TEST);
    gl.blendFunc (gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    var framebuffer = gl.createFramebuffer ();
    gl.bindFramebuffer (gl.FRAMEBUFFER, framebuffer);

    var texture = gl.createTexture ();
    gl.bindTexture (gl.TEXTURE_2D, texture);
    gl.texImage2D  (gl.TEXTURE_2D, 0, gl.RGBA, FRAMEBUFFER_SIZE, FRAMEBUFFER_SIZE, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);

    var renderbuffer = gl.createRenderbuffer ();
    gl.bindRenderbuffer    (gl.RENDERBUFFER, renderbuffer);
    gl.renderbufferStorage (gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, FRAMEBUFFER_SIZE, FRAMEBUFFER_SIZE);

    gl.framebufferTexture2D    (gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
    gl.framebufferRenderbuffer (gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, renderbuffer);

    gl.bindTexture (gl.TEXTURE_2D, null);
    gl.bindRenderbuffer (gl.RENDERBUFFER, null);

    var mvMatrix = mat4.create (), pMatrix = mat4.create (), pPMatrix = mat4.create ();

    initController (vnn, canvas, mvMatrix);

    var network = new Network (gl, vnn);

    requestAnimationFrame (draw);

    function draw (timestamp)
    {
        var time = timestamp / 1000.0;

        gl.bindFramebuffer (gl.FRAMEBUFFER, null);
        gl.viewport (0, 0, canvas.width, canvas.height);
        gl.clearColor (vnn.params.clear_color[0], vnn.params.clear_color[1], vnn.params.clear_color[2], 1.0);
        gl.clear (gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.enable (gl.BLEND);
        network.drawNodes       (pMatrix, mvMatrix, time);
        network.drawConnections (pMatrix, mvMatrix);
        network.drawSpikes      (pMatrix, mvMatrix, time);

        gl.bindFramebuffer (gl.FRAMEBUFFER, framebuffer);
        gl.viewport (0, 0, FRAMEBUFFER_SIZE, FRAMEBUFFER_SIZE);
        gl.clearColor (1.0, 1.0, 1.0, 1.0);
        gl.disable (gl.BLEND);
        gl.clear (gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        network.drawPicker (pMatrix, mvMatrix);

        requestAnimationFrame (draw);
    }

    function webGLIsNotSupported ()
    {
        try       { return !window.WebGLRenderingContext || !canvas.getContext ('webgl'); }
        catch (e) { return true; }
    };

    vnn.messageReceived.add (function (buffer)
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

    vnn.windowResized.add (function ()
    {
        canvas.width  = viewport.clientWidth;
        canvas.height = viewport.clientHeight;

        mat4.perspective (pMatrix, Math.PI / 4, canvas.width / canvas.height, vnn.params.near, vnn.params.far);
    });

    vnn.pickerClicked.add (function (x, y)
    {
        var bytes = new Uint8Array (4);
        gl.readPixels (x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
        var id = bytes[0] + bytes[1] * 256 + bytes[2] * 65536;
        vnn.selectNode (id);
    });

    vnn.pickerMoved.add (function (x, y)
    {
        var bytes = new Uint8Array (4);
        gl.readPixels (x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
        var id = bytes[0] + bytes[1] * 256 + bytes[2] * 65536;
        network.hover (id);
    });

    if (webGLIsNotSupported ())
    {
        $('#viewport').html ("<div style='height:100%; width:100%; display:table;'>"
                + "<div style='display:table-cell; vertical-align:middle; text-align:center;'>"
                + "Your browser doesn't support <a href='http://caniuse.com/webgl'>WebGL</a></div></div>");
        throw 'WebGL not supported!';
    }
}
