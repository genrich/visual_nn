function Network (gl)
{
    this.get = function (i)
    {
        var i_start = i * NODE_SIZE;
        return vec3.fromValues (nodes[i_start], nodes[i_start + 1], nodes[i_start + 2]);
    }

    this.set = function (i, x, y, z)
    {
        var i_start = i       * NODE_SIZE;
        var i_end   = i_start + NODE_SIZE;
        nodes[i_start]     = x;
        nodes[i_start + 1] = y;
        nodes[i_start + 2] = z;
        nodes[i_start + 3] = 0;

        if (connections[i] === undefined) connections[i] = [];

        if (i_end <= nodesArray.length)
        {
            updateVertexData (i_start, i_end, x, y, z);
            if (i >= numNodes)
                numNodes = i + 1;
        }
    }

    this.connect = function (from, to)
    {
        if (from == to)
            return;

        if (connections[from] === undefined) connections[from] = [];
        if (connections[to]   === undefined) connections[to]   = [];

        var found = false;
        for (var i = 0; i < connections[from].length; i += 2)
            if (connections[from][i] == to)
            {
                found = true;
                break;
            }

        if (!found)
        {
            connections[from].push (to, numLines);
            connections[to].push (from, numLines);
            lines.push (from, to);

            var i_start = numLines * 2;
            var i_end   = i_start  + 2;

            if (i_end <= linesArray.length)
                updateLineData (i_start, i_end, from, to);

            ++numLines;
        }
    }

    this.spike = function (v_idx)
    {
        var time = performance.now () / 1000;

        if (v_idx < numNodes)
        {
            var i_start = v_idx * NODE_SIZE + 3;
            nodesArray[i_start] = time + SPIKE_DURATION;

            nodeSpikes.enqueue (v_idx);
        }

        var newSpikes = [];
        for (var i = 0; i < connections[v_idx].length; i += 2)
        {
            var u_idx = connections[v_idx][i];
            var v = this.get (v_idx), u = this.get (u_idx);
            var duration = vec3.distance (v, u) / SPIKE_SPEED,
                end_time = time + duration;

            newSpikes.push (v[0]); newSpikes.push (v[1]); newSpikes.push (v[2]);
            newSpikes.push (u[0]); newSpikes.push (u[1]); newSpikes.push (u[2]);
            newSpikes.push (duration);
            newSpikes.push (end_time);
        }
        updateSpikesArray (newSpikes);
    }

    this.drawNodes = function (pMatrix, mvMatrix, time)
    {
        gl.useProgram (nodeProgram);
        gl.enableVertexAttribArray (nodeProgram.position);
        gl.enableVertexAttribArray (nodeProgram.end_time);

        gl.uniformMatrix4fv (nodeProgram.pMatrix,  false, pMatrix);
        gl.uniformMatrix4fv (nodeProgram.mvMatrix, false, mvMatrix);
        gl.uniform1f        (nodeProgram.time, time);
        gl.uniform1f        (nodeProgram.duration, SPIKE_DURATION);
        gl.uniform3fv       (nodeProgram.rest_color,  REST_COLOR);
        gl.uniform3fv       (nodeProgram.spike_color, SPIKE_COLOR);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodeBuffer);

        while (!nodeSpikes.isEmpty ()) // update buffer data with spike end_time
        {
            var i_start = nodeSpikes.dequeue () * NODE_SIZE + 3;
            gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, nodesArray.subarray (i_start, i_start + 1));
        }

        if (nodes.length > nodesArray.length) // reallocate
        {
            numNodes = nodes.length / NODE_SIZE;

            var i_start = nodesArray.length;

            var a = new Float32Array (nodes.length + NODE_BUF_INC);
            a.set (nodesArray);
            nodesArray = a;

            nodesArray.set (nodes.slice (i_start), i_start);

            gl.bufferData (gl.ARRAY_BUFFER, nodesArray, gl.DYNAMIC_DRAW);
        }
        gl.vertexAttribPointer (nodeProgram.position, VEC3_SIZE, gl.FLOAT, false, 16, 0);
        gl.vertexAttribPointer (nodeProgram.end_time,         1, gl.FLOAT, false, 16, 12);

        gl.drawArrays (gl.POINTS, 0, numNodes);
    }

    this.drawConnections = function (pMatrix, mvMatrix)
    {
        gl.useProgram (connectionProgram);
        gl.enableVertexAttribArray (connectionProgram.position);

        gl.uniformMatrix4fv (connectionProgram.pMatrix,  false, pMatrix);
        gl.uniformMatrix4fv (connectionProgram.mvMatrix, false, mvMatrix);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodeBuffer);
        gl.vertexAttribPointer (connectionProgram.position, VEC3_SIZE, gl.FLOAT, false, 20, 0);

        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);

        if (lines.length > linesArray.length) // reallocate
        {
            numLines = lines.length / 2;

            var i_start = linesArray.length;

            var a = new Uint16Array (lines.length + LINE_BUF_INC);
            a.set (linesArray);
            linesArray = a;

            linesArray.set (lines.slice (i_start), i_start);

            gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, linesArray, gl.DYNAMIC_DRAW);
        }

        gl.drawElements (gl.LINES, numLines * 2, gl.UNSIGNED_SHORT, 0);
    }

    this.drawSpikes = function (pMatrix, mvMatrix, time) // draw spike propagation
    {
        gl.useProgram (spikeProgram);
        gl.enableVertexAttribArray (spikeProgram.position);
        gl.enableVertexAttribArray (spikeProgram.end_position);
        gl.enableVertexAttribArray (spikeProgram.duration);
        gl.enableVertexAttribArray (spikeProgram.end_time);

        gl.uniformMatrix4fv (spikeProgram.pMatrix,  false, pMatrix);
        gl.uniformMatrix4fv (spikeProgram.mvMatrix, false, mvMatrix);
        gl.uniform1f        (spikeProgram.time, time);

        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        gl.vertexAttribPointer (spikeProgram.position,     VEC3_SIZE, gl.FLOAT, false, 32, 0);
        gl.vertexAttribPointer (spikeProgram.end_position, VEC3_SIZE, gl.FLOAT, false, 32, 12);
        gl.vertexAttribPointer (spikeProgram.duration,             1, gl.FLOAT, false, 32, 24);
        gl.vertexAttribPointer (spikeProgram.end_time,             1, gl.FLOAT, false, 32, 28);

        clearExpiredSpikes (time);

        gl.drawArrays (gl.POINTS, 0, numSpikes);
    }

    function clearExpiredSpikes (time)
    {
        // console.log ('heap=' + heap.size ()
        //              + ' spikes=' + spikesArray.length
        //              + ' poolLen=' + edgeSpikesPool.length ()
        //              + ' poolSize=' + edgeSpikesPool.size ()
        //              + ' numSpikes=' + numSpikes);

        var limit = 0;
        while (++limit < 100 && heap.isTopExpired (time))
        {
            var expiredSpikeIdx = heap.pop ();

            if (expiredSpikeIdx == (numSpikes - 1))
                --numSpikes;
            else
                edgeSpikesPool.enqueue (expiredSpikeIdx);
        }
    }

    function updateSpikesArray (spikes)
    {
        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        while (!edgeSpikesPool.isEmpty () && spikes.length > 0) // reuse expired spikes
        {
            var reuseSpikeIdx = edgeSpikesPool.dequeue ();

            var end_time = spikes[spikes.length - 1]; // last spike item component is end_time
            heap.push (end_time, reuseSpikeIdx);

            var i_start = reuseSpikeIdx * SPIKE_ITEM_SIZE;
                i_end   = i_start       + SPIKE_ITEM_SIZE;
            spikesArray.set (spikes.slice (spikes.length - SPIKE_ITEM_SIZE), i_start);
            gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, spikesArray.subarray (i_start, i_end));

            spikes.length -= SPIKE_ITEM_SIZE;
        }

        if (spikes.length == 0)
            return;

        var i_start = numSpikes * SPIKE_ITEM_SIZE,
            i_end   = i_start + spikes.length;

        for (var i = 0; i < spikes.length; i += SPIKE_ITEM_SIZE)
        {
            var end_time = spikes[i + SPIKE_ITEM_SIZE - 1]; // last spike item component is end_time
            heap.push (end_time, numSpikes++);
        }

        if (i_end > spikesArray.length)
        {
            var a = new Float32Array (i_end + SPIKE_BUF_INC);
            a.set (spikesArray);
            spikesArray = a;

            spikesArray.set (spikes, i_start);
            gl.bufferData (gl.ARRAY_BUFFER, spikesArray, gl.DYNAMIC_DRAW);
        }
        else
        {
            spikesArray.set (spikes, i_start);
            gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, spikesArray.subarray (i_start, i_end));
        }
    }

    function updateVertexData (i_start, i_end, x, y, z)
    {
        gl.bindBuffer (gl.ARRAY_BUFFER, nodeBuffer);

        nodesArray.set ([x, y, z], i_start);
        var nodesSubArray = nodesArray.subarray (i_start, i_end);

        gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, nodesSubArray);
    }

    function updateLineData (i_start, i_end, from, to)
    {
        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);

        linesArray.set ([from, to], i_start);
        var linesSubArray = linesArray.subarray (i_start, i_end);

        gl.bufferSubData (gl.ELEMENT_ARRAY_BUFFER, i_start * INT_SIZE, linesSubArray);
    }

    function makeShader (gl, src, type)
    {
        var shader = gl.createShader (type);
        gl.shaderSource (shader, src);
        gl.compileShader (shader);

        if (!gl.getShaderParameter (shader, gl.COMPILE_STATUS))
            throw new Error ('Error compiling shader: ' + gl.getShaderInfoLog (shader));

        return shader;
    }

    function initProgram (gl, name)
    {
        var fs_source = null, vs_source = null;

        var xhr = new XMLHttpRequest ();
        xhr.overrideMimeType ('text/plain');

        xhr.open ('GET', './shaders/' + name + 'Vertex.cl', false);
        xhr.send (null);

        if (xhr.readyState == xhr.DONE)
        {
            if (xhr.status === 200)
                vs_source = xhr.responseText;
            else
                throw new Error ('Error retrieving vertex shader source: ' + xhr.statusText);
        }

        xhr.open ('GET', './shaders/' + name + 'Fragment.cl', false);
        xhr.send (null);

        if (xhr.readyState == xhr.DONE) {
            if (xhr.status === 200)
                fs_source = xhr.responseText;
            else
                throw new Error ('Error retrieving fragment shader source: ' + xhr.statusText);
        }

        var vertexShader   = makeShader (gl, vs_source, gl.VERTEX_SHADER);
        var fragmentShader = makeShader (gl, fs_source, gl.FRAGMENT_SHADER);

        var program = gl.createProgram ();

        gl.attachShader (program, vertexShader);
        gl.attachShader (program, fragmentShader);
        gl.linkProgram (program);

        if (!gl.getProgramParameter (program, gl.LINK_STATUS))
            throw new Error ('Unable to initialize the shader program.');

        program.pMatrix  = gl.getUniformLocation (program, 'pMatrix');
        program.mvMatrix = gl.getUniformLocation (program, 'mvMatrix');

        program.position = gl.getAttribLocation (program, 'position');

        return program;
    }

    const SPIKE_ITEM_SIZE = 8, // x1, y1, z1, x2, y2, z3, duration, end_time
          VEC3_SIZE = 3, // x, y, z
          NODE_SIZE = 4, // x, y, z, end_time
          NODE_BUF_INC = 0, LINE_BUF_INC = 0, SPIKE_BUF_INC = 0,
          FLOAT_SIZE = 4, INT_SIZE = 2,
          SPIKE_SPEED = 100,
          REST_COLOR = vec3.fromValues (0.2, 0.2, 0.2), SPIKE_COLOR = vec3.fromValues (0.8, 0.8, 0),
          SPIKE_DURATION = 3.0;

    // node array used to accumulate position data which is transfered in the draw method to the typed array and gl vertex buffer
    // nodes = [x, y, z, end_time, ...]
    var nodes = [], nodesArray = new Float32Array (NODE_BUF_INC), numNodes = 0;
    var nodeSpikes = new PseudoQueue (); // queue for nodes which are firing
    // adjacency list, connections[v_idx] = [u_idx : adjacent_vertex (v_idx), e_idx : corresponding_edge_idx (v_idx, u_idx), ...]
    var connections = [];
    // edge list, lines = [v_idx, u_idx, ...]
    var lines = [], totalLines = 0, linesArray = new Uint16Array (LINE_BUF_INC), numLines = 0;
    // spike array, [pos_start, pos_end, duration, end_time]
    var edgeSpikesPool = new PseudoQueue (), spikesArray = new Float32Array (SPIKE_BUF_INC), numSpikes = 0;

    var nodeProgram       = initProgram (gl, 'node'),
        connectionProgram = initProgram (gl, 'connection'),
        spikeProgram      = initProgram (gl, 'spike');

    var heap = new MinBinaryHeap ();

    var nodeBuffer      = gl.createBuffer (),
        lineIndexBuffer = gl.createBuffer (),
        spikeBuffer     = gl.createBuffer ();

    nodeProgram.end_time = gl.getAttribLocation (nodeProgram, 'end_time');
    nodeProgram.time        = gl.getUniformLocation (nodeProgram, 'time');
    nodeProgram.duration    = gl.getUniformLocation (nodeProgram, 'duration');
    nodeProgram.rest_color  = gl.getUniformLocation (nodeProgram, 'rest_color');
    nodeProgram.spike_color = gl.getUniformLocation (nodeProgram, 'spike_color');

    spikeProgram.end_position = gl.getAttribLocation (spikeProgram, 'end_position');
    spikeProgram.duration     = gl.getAttribLocation (spikeProgram, 'duration');
    spikeProgram.end_time     = gl.getAttribLocation (spikeProgram, 'end_time');
    spikeProgram.time = gl.getUniformLocation (spikeProgram, 'time');

    gl.bindBuffer (gl.ARRAY_BUFFER, nodeBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, nodesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);
    gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, linesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, spikesArray, gl.DYNAMIC_DRAW);
}
