function Network (gl, params)
{
    const FLOAT_SIZE = 4, INT_SIZE = 2,
          SPIKE_ITEM_SIZE = 8, // x1, y1, z1, x2, y2, z3, duration, end_time
          VEC3_SIZE = 3, // x, y, z
          NODE_SIZE = 4, // x, y, z, end_time
          NODE_BYTES = NODE_SIZE * FLOAT_SIZE,
          NODE_BUF_INC = 0, EDGE_BUF_INC = 0, SPIKE_BUF_INC = 0;

    // node array used to accumulate position data which is transfered in the draw method to the typed array and gl vertex buffer
    // nodesArray = [x, y, z, end_time, ...]
    var numNodes = 0, nodesArray = new Float32Array (NODE_BUF_INC), nodesToUpdate = [], isNodesBufferFresh = true, nodeSpikes = [];
    // adjacency list, adjacencyList[v_idx] = [u_idx : adjacent_vertex (v_idx), e_idx : corresponding_edge_idx (v_idx, u_idx), ...]
    var adjacencyList = [];
    // edge list, edges = [v_idx, u_idx, ...]
    var edges = [], numEdges = 0; edgesArray = new Uint16Array (EDGE_BUF_INC);
    // spike array, [pos_start, pos_end, duration, end_time]
    var edgeSpikesPool = new PseudoQueue (), spikesArray = new Float32Array (SPIKE_BUF_INC), numSpikes = 0, radiatingSpikes = [];

    var nodeProgram       = initProgram (gl, 'node'),
        connectionProgram = initProgram (gl, 'connection'),
        spikeProgram      = initProgram (gl, 'spike');

    var heap = new MinBinaryHeap ();

    var nodesBuffer     = gl.createBuffer (),
        lineIndexBuffer = gl.createBuffer (),
        spikeBuffer     = gl.createBuffer ();

    nodeProgram.end_time = gl.getAttribLocation (nodeProgram, 'end_time');
    nodeProgram.time          = gl.getUniformLocation (nodeProgram, 'time');
    nodeProgram.attenuation   = gl.getUniformLocation (nodeProgram, 'attenuation');
    nodeProgram.rest_color    = gl.getUniformLocation (nodeProgram, 'rest_color');
    nodeProgram.spike_color   = gl.getUniformLocation (nodeProgram, 'spike_color');

    connectionProgram.rest_color = gl.getUniformLocation (connectionProgram, 'rest_color');

    spikeProgram.end_position = gl.getAttribLocation (spikeProgram, 'end_position');
    spikeProgram.duration     = gl.getAttribLocation (spikeProgram, 'duration');
    spikeProgram.end_time     = gl.getAttribLocation (spikeProgram, 'end_time');
    spikeProgram.time        = gl.getUniformLocation (spikeProgram, 'time');
    spikeProgram.spike_color = gl.getUniformLocation (spikeProgram, 'spike_color');

    gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, nodesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);
    gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, edgesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, spikesArray, gl.DYNAMIC_DRAW);

    this.get = function (i)
    {
        var i_start = i * NODE_SIZE;
        return vec3.fromValues (nodesArray[i_start], nodesArray[i_start + 1], nodesArray[i_start + 2]);
    }

    this.set = function (i, x, y, z)
    {
        var i_start = i * NODE_SIZE;

        if (numNodes <= i)
        {
            if (nodesArray.length < i_start + NODE_SIZE)
                nodesArray = reallocateFloatArray (nodesArray, i_start + NODE_SIZE + NODE_BUF_INC);
            numNodes = i + 1;
            isNodesBufferFresh = false;
        }

        nodesArray[i_start]     = x;
        nodesArray[i_start + 1] = y;
        nodesArray[i_start + 2] = z;
        nodesArray[i_start + 3] = 0;

        nodesToUpdate.push (i);

        if (adjacencyList[i] === undefined) adjacencyList[i] = [];
    }

    function prefill (id1, id2)
    {
        var max = Math.max (id1, id2);
        for (var id = numNodes; id <= max; ++id)
            this.set (id, 0, 0, 0);
    }

    this.connect = function (from, to)
    {
        if (from == to) return;

        if (adjacencyList[from] === undefined) adjacencyList[from] = [];

        // skip existing connections
        for (var i = 0; i < adjacencyList[from].length; i += 2)
            if (adjacencyList[from][i] == to)
                return;

        adjacencyList[from].push (to, numEdges);
        edges.push (from, to);

        prefill (from, to);

        var i_start = numEdges * 2;
        var i_end   = i_start  + 2;

        if (i_end <= edgesArray.length)
            updateLineData (i_start, i_end, from, to);

        ++numEdges;
    }

    this.spike = function (v_idx)
    {
        if (adjacencyList[v_idx] === undefined) return;

        var time = performance.now () / 1000;

        if (v_idx < numNodes)
        {
            var i_start = v_idx * NODE_SIZE + 3;
            nodesArray[i_start] = time + params.spike_attenuation;

            nodeSpikes.push (v_idx);
        }

        for (var i = 0; i < adjacencyList[v_idx].length; i += 2)
        {
            var u_idx = adjacencyList[v_idx][i];
            var v = this.get (v_idx), u = this.get (u_idx);
            var duration = vec3.distance (v, u) / params.spike_speed,
                end_time = time + duration;

            radiatingSpikes.push (v[0], v[1], v[2], u[0], u[1], u[2], duration, end_time);
        }
    }

    this.drawNodes = function (pMatrix, mvMatrix, time)
    {
        useProgram (nodeProgram, pMatrix, mvMatrix);
        gl.enableVertexAttribArray (nodeProgram.end_time);

        gl.uniform1f (nodeProgram.time,        time);
        gl.uniform1f (nodeProgram.attenuation, params.spike_attenuation);
        gl.uniform3fv (nodeProgram.rest_color,  params.rest_color);
        gl.uniform3fv (nodeProgram.spike_color, params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);

        updateNodes ();
        updateSpikesEndTime ();

        gl.vertexAttribPointer (nodeProgram.position, VEC3_SIZE, gl.FLOAT, false, NODE_BYTES, 0);
        gl.vertexAttribPointer (nodeProgram.end_time,         1, gl.FLOAT, false, NODE_BYTES, 12);

        gl.drawArrays (gl.POINTS, 0, numNodes);
    }

    this.drawConnections = function (pMatrix, mvMatrix)
    {
        useProgram (connectionProgram, pMatrix, mvMatrix);

        gl.uniform3fv (connectionProgram.rest_color, params.rest_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
        gl.vertexAttribPointer (connectionProgram.position, VEC3_SIZE, gl.FLOAT, false, NODE_BYTES, 0);

        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);

        if (edges.length > edgesArray.length) // reallocate
        {
            numEdges = edges.length / 2;

            var i_start = edgesArray.length;

            var a = new Uint16Array (edges.length + EDGE_BUF_INC);
            a.set (edgesArray);
            edgesArray = a;

            edgesArray.set (edges.slice (i_start), i_start);

            gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, edgesArray, gl.DYNAMIC_DRAW);
        }

        gl.drawElements (gl.LINES, numEdges * 2, gl.UNSIGNED_SHORT, 0);
    }

    this.drawSpikes = function (pMatrix, mvMatrix, time) // draw spike propagation
    {
        useProgram (spikeProgram, pMatrix, mvMatrix);
        gl.enableVertexAttribArray (spikeProgram.end_position);
        gl.enableVertexAttribArray (spikeProgram.duration);
        gl.enableVertexAttribArray (spikeProgram.end_time);

        gl.uniform1f        (spikeProgram.time, time);
        gl.uniform3fv       (spikeProgram.spike_color, params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        clearExpiredSpikes (time);
        updateSpikesArray ();

        gl.vertexAttribPointer (spikeProgram.position,     VEC3_SIZE, gl.FLOAT, false, 32, 0);
        gl.vertexAttribPointer (spikeProgram.end_position, VEC3_SIZE, gl.FLOAT, false, 32, 12);
        gl.vertexAttribPointer (spikeProgram.duration,             1, gl.FLOAT, false, 32, 24);
        gl.vertexAttribPointer (spikeProgram.end_time,             1, gl.FLOAT, false, 32, 28);

        gl.drawArrays (gl.POINTS, 0, numSpikes);
    }

    function updateNodes ()
    {
        if (isNodesBufferFresh)
        {
            while (nodesToUpdate.length)
            {
                var i_start = nodesToUpdate.pop () * NODE_SIZE;

                var nodesSubArray = nodesArray.subarray (i_start, i_start + NODE_SIZE);
                gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, nodesSubArray);
            }
        }
        else
        {
            gl.bufferData (gl.ARRAY_BUFFER, nodesArray, gl.DYNAMIC_DRAW);
            isNodesBufferFresh = true;
        }
    }

    function updateSpikesEndTime ()
    {
        while (nodeSpikes.length)
        {
            var i_start = nodeSpikes.pop () * NODE_SIZE + 3;
            var a = nodesArray.subarray (i_start, i_start + 1);
            if (a.length)
                gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, a);
        }

    }

    function useProgram (program, pMatrix, mvMatrix)
    {
        gl.useProgram (program);
        gl.enableVertexAttribArray (program.position);

        gl.uniformMatrix4fv (program.pMatrix,  false, pMatrix);
        gl.uniformMatrix4fv (program.mvMatrix, false, mvMatrix);
        gl.uniform1f (program.log_far_const, params.log_far_const);
        gl.uniform1f (program.far,           params.far);
        gl.uniform3fv (program.clear_color, params.clear_color);
    }

    function clearExpiredSpikes (time)
    {
        while (heap.isTopExpired (time))
        {
            var expiredSpikeIdx = heap.pop ();

            if (expiredSpikeIdx == (numSpikes - 1))
                --numSpikes;
            else
                edgeSpikesPool.enqueue (expiredSpikeIdx);
        }
    }

    function updateSpikesArray ()
    {
        while (!edgeSpikesPool.isEmpty () && radiatingSpikes.length) // reuse expired spikes
        {
            var reuseSpikeIdx = edgeSpikesPool.dequeue ();

            var end_time = radiatingSpikes[radiatingSpikes.length - 1]; // last spike item component is end_time
            heap.push (end_time, reuseSpikeIdx);

            var i_start = reuseSpikeIdx * SPIKE_ITEM_SIZE;
                i_end   = i_start       + SPIKE_ITEM_SIZE;
            spikesArray.set (radiatingSpikes.slice (radiatingSpikes.length - SPIKE_ITEM_SIZE), i_start);
            gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, spikesArray.subarray (i_start, i_end));

            radiatingSpikes.length -= SPIKE_ITEM_SIZE;
        }

        if (radiatingSpikes.length == 0)
            return;

        var i_start = numSpikes * SPIKE_ITEM_SIZE,
            i_end   = i_start + radiatingSpikes.length;

        for (var i = 0; i < radiatingSpikes.length; i += SPIKE_ITEM_SIZE)
        {
            var end_time = radiatingSpikes[i + SPIKE_ITEM_SIZE - 1]; // last spike item component is end_time
            heap.push (end_time, numSpikes++);
        }

        if (i_end > spikesArray.length)
        {
            spikesArray = reallocateFloatArray (spikesArray, i_end + SPIKE_BUF_INC);

            spikesArray.set (radiatingSpikes, i_start);
            gl.bufferData (gl.ARRAY_BUFFER, spikesArray, gl.DYNAMIC_DRAW);
        }
        else
        {
            spikesArray.set (radiatingSpikes, i_start);
            gl.bufferSubData (gl.ARRAY_BUFFER, i_start * FLOAT_SIZE, spikesArray.subarray (i_start, i_end));
        }
        radiatingSpikes.length = 0;
    }

    function updateLineData (i_start, i_end, from, to)
    {
        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, lineIndexBuffer);

        edgesArray.set ([from, to], i_start);
        var linesSubArray = edgesArray.subarray (i_start, i_end);

        gl.bufferSubData (gl.ELEMENT_ARRAY_BUFFER, i_start * INT_SIZE, linesSubArray);
    }

    function reallocateFloatArray (oldArray, newLength)
    {
        var newArray = new Float32Array (newLength);
        newArray.set (oldArray);
        return newArray;
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

        program.pMatrix       = gl.getUniformLocation (program, 'pMatrix');
        program.mvMatrix      = gl.getUniformLocation (program, 'mvMatrix');
        program.log_far_const = gl.getUniformLocation (program, 'log_far_const');
        program.far           = gl.getUniformLocation (program, 'far');
        program.clear_color   = gl.getUniformLocation (program, 'clear_color');

        program.position = gl.getAttribLocation (program, 'position');

        return program;
    }
}
