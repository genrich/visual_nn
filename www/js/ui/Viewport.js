function initViewport (sig)
{
    function draw (timestamp)
    {
        var time = timestamp / 1000.0;

        gl.clear (gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        network.drawNodes       (pMatrix, mvMatrix, time);
        // network.drawConnections (pMatrix, mvMatrix);
        // network.drawSpikes      (pMatrix, mvMatrix, time_sec);

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

        if (type == CONST.STIMULUS_POS)
        {
            var id = new Uint32Array (buffer, 4, 1)[0];
            var pos = new Float32Array (buffer, 8,  3);

            network.set (id, pos[0], pos[1], pos[2]);
        }
        else if (type == CONST.STIMULUS_SPIKE)
        {
            var id = new Uint32Array (buffer, 4, 1)[0];

            network.spike (id);
        }
        else if (type == CONST.SOMA_POS)
        {
            var id = new Uint32Array (buffer, 4, 1) [0]
            var pos = new Float32Array (buffer, 8, 3)

            var geom = neurons.geometry
            geom.vertices[id].set (pos[0], pos[1], pos[2])
            geom.colors[id].copy (COLOR_REST)
            geom.verticesNeedUpdate = true
            geom.colorsNeedUpdate   = true
        }
        else if (type == CONST.CONNECTION)
        {
            var ints = new Uint32Array (buffer, 4, 2)
            var id = ints[0]
            var verticesCount = ints[1]

            connSegCounts[id] = verticesCount / 2
            connSegLengths[id] = new Float32Array (buffer, 12, 1) [0]

            var vertices = new Float32Array (buffer, 16, verticesCount * 3)

            if (!connIdMap[id])
            {
                connIdMap[id] = connLastIdx
                connLastIdx += verticesCount

                connCoef = newZeroArray (connLastIdx)

                var geom = new THREE.Geometry ()
                geom.vertices = cloneFill (connections.geometry.vertices, DUMMY_POINT, connLastIdx)
                geom.colors = cloneFill (connections.geometry.colors, COLOR_REST, connLastIdx)

                for (var i = 0; i < verticesCount; i++)
                {
                    geom.vertices[connIdMap[id] + i] = new THREE.Vector3 (vertices[i*3], vertices[i*3 + 1], vertices[i*3 + 2])
                    geom.colors[connIdMap[id] + i] = COLOR_REST.clone ()
                }

                scene.remove (connections)
                connections = new THREE.Line (geom, new THREE.LineBasicMaterial ({vertexColors: THREE.VertexColors}), THREE.LinePieces)
                scene.add (connections)
            }
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

    gl.clearColor (0.9, 0.9, 0.9, 1.0);
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
