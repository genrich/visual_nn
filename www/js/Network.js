function Network (gl, vnn)
{
    const FLOAT_SIZE = 4, INT_SIZE = 2,
          // x, y, z
          POS_SIZE = 3,
          // x1, y1, z1, x2, y2, z3, duration, end_time
          SPIKE_ITEM_SIZE            = 8,
          SPIKE_POS1_OFFSET          = 0,
          SPIKE_POS2_OFFSET          = 3,
          SPIKE_DURATION_OFFSET      = 6,
          SPIKE_END_TIME_OFFSET      = 7,
          SPIKE_BYTES                = SPIKE_ITEM_SIZE       * FLOAT_SIZE,
          SPIKE_POS1_BYTE_OFFSET     = SPIKE_POS1_OFFSET     * FLOAT_SIZE,
          SPIKE_POS2_BYTE_OFFSET     = SPIKE_POS2_OFFSET     * FLOAT_SIZE,
          SPIKE_DURATION_BYTE_OFFSET = SPIKE_DURATION_OFFSET * FLOAT_SIZE,
          SPIKE_END_TIME_BYTE_OFFSET = SPIKE_END_TIME_OFFSET * FLOAT_SIZE,
          // x, y, z, id, end_time, attributes
          NODE_SIZE                 = 6,
          NODE_POS_OFFSET           = 0,
          NODE_ID_OFFSET            = 3,
          NODE_END_TIME_OFFSET      = 4,
          NODE_ATTR_OFFSET          = 5,
          NODE_BYTES                = NODE_SIZE            * FLOAT_SIZE,
          NODE_POS_BYTE_OFFSET      = NODE_POS_OFFSET      * FLOAT_SIZE,
          NODE_ID_BYTE_OFFSET       = NODE_ID_OFFSET       * FLOAT_SIZE,
          NODE_END_TIME_BYTE_OFFSET = NODE_END_TIME_OFFSET * FLOAT_SIZE,
          NODE_ATTR_BYTE_OFFSET     = NODE_ATTR_OFFSET     * FLOAT_SIZE,
          //
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
    var heap = new MinBinaryHeap ();


    var nodeProgram       = initProgram (gl, 'node'),
        nodePickerProgram = initProgram (gl, 'nodePicker'),
        connectionProgram = initProgram (gl, 'connection'),
        spikeProgram      = initProgram (gl, 'spike');

    var nodesBuffer     = gl.createBuffer (),
        lineIndexBuffer = gl.createBuffer (),
        spikeBuffer     = gl.createBuffer ();

    nodeProgram.end_time   = gl.getAttribLocation (nodeProgram, 'end_time');
    nodeProgram.attributes = gl.getAttribLocation (nodeProgram, 'attributes');
    nodeProgram.time        = gl.getUniformLocation (nodeProgram, 'time');
    nodeProgram.attenuation = gl.getUniformLocation (nodeProgram, 'attenuation');
    nodeProgram.rest_color  = gl.getUniformLocation (nodeProgram, 'rest_color');
    nodeProgram.spike_color = gl.getUniformLocation (nodeProgram, 'spike_color');
    nodeProgram.hover_color = gl.getUniformLocation (nodeProgram, 'hover_color');
    nodeProgram.point_size  = gl.getUniformLocation (nodeProgram, 'point_size');

    nodePickerProgram.id = gl.getAttribLocation (nodePickerProgram, 'id');
    nodePickerProgram.point_size = gl.getUniformLocation (nodePickerProgram, 'point_size');

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

    this.clear = function ()
    {
        numNodes = 0; nodesToUpdate = [];
        numSpikes = 0; nodeSpikes = [];
        adjacencyList = [];
        numEdges = 0; edges = [];
        edgeSpikesPool = new PseudoQueue ();
        radiatingSpikes = [];
        heap = new MinBinaryHeap ();
    }

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

        nodesArray[i_start + NODE_POS_OFFSET]      = x;
        nodesArray[i_start + NODE_POS_OFFSET + 1]  = y;
        nodesArray[i_start + NODE_POS_OFFSET + 2]  = z;
        nodesArray[i_start + NODE_ID_OFFSET]       = i;
        nodesArray[i_start + NODE_END_TIME_OFFSET] = 0;
        nodesArray[i_start + NODE_ATTR_OFFSET]     = 0;

        nodesToUpdate.push (i);

        if (adjacencyList[i] === undefined) adjacencyList[i] = [];
    }

    var lastHoverId;
    this.hover = function (id)
    {
        if (id < CONST.MAX_ID)
        {
            if (lastHoverId == undefined || id != lastHoverId)
            {
                nodesArray[id * NODE_SIZE + NODE_ATTR_OFFSET] = 1;
                nodesToUpdate.push (id);
            }
            if (lastHoverId != id)
            {
                nodesArray[lastHoverId * NODE_SIZE + NODE_ATTR_OFFSET] = 0;
                nodesToUpdate.push (lastHoverId);
                lastHoverId = undefined;
            }
            lastHoverId = id;
        }
        else if (lastHoverId)
        {
            nodesArray[lastHoverId * NODE_SIZE + NODE_ATTR_OFFSET] = 0;
            nodesToUpdate.push (lastHoverId);
            lastHoverId = undefined;
        }
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
            var i = v_idx * NODE_SIZE + NODE_END_TIME_OFFSET;
            nodesArray[i] = time + vnn.params.spike_attenuation;

            nodeSpikes.push (v_idx);
        }

        for (var i = 0; i < adjacencyList[v_idx].length; i += 2)
        {
            var u_idx = adjacencyList[v_idx][i];
            var v = this.get (v_idx), u = this.get (u_idx);
            var duration = vec3.distance (v, u) / vnn.params.spike_speed,
                end_time = time + duration;

            radiatingSpikes.push (v[0], v[1], v[2], u[0], u[1], u[2], duration, end_time);
        }
    }

    this.drawNodes = function (pMatrix, mvMatrix, time)
    {
        useProgram (nodeProgram, pMatrix, mvMatrix);
        gl.enableVertexAttribArray (nodeProgram.end_time);
        gl.enableVertexAttribArray (nodeProgram.attributes);

        gl.uniform1f (nodeProgram.time,        time);
        gl.uniform1f (nodeProgram.attenuation, vnn.params.spike_attenuation);
        gl.uniform1f (nodeProgram.point_size,  vnn.params.point_size);
        gl.uniform3fv (nodeProgram.rest_color,  vnn.params.rest_color);
        gl.uniform3fv (nodeProgram.spike_color, vnn.params.spike_color);
        gl.uniform3fv (nodeProgram.hover_color, vnn.params.hover_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);

        updateNodes ();
        updateSpikesEndTime ();

        gl.vertexAttribPointer (nodeProgram.position,   POS_SIZE, gl.FLOAT, false, NODE_BYTES, NODE_POS_BYTE_OFFSET);
        gl.vertexAttribPointer (nodeProgram.end_time,          1, gl.FLOAT, false, NODE_BYTES, NODE_END_TIME_BYTE_OFFSET);
        gl.vertexAttribPointer (nodeProgram.attributes,        1, gl.FLOAT, false, NODE_BYTES, NODE_ATTR_BYTE_OFFSET);

        gl.drawArrays (gl.POINTS, 0, numNodes);
    }

    this.drawConnections = function (pMatrix, mvMatrix)
    {
        useProgram (connectionProgram, pMatrix, mvMatrix);

        gl.uniform3fv (connectionProgram.rest_color, vnn.params.connection_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
        gl.vertexAttribPointer (connectionProgram.position, POS_SIZE, gl.FLOAT, false, NODE_BYTES, NODE_POS_BYTE_OFFSET);

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
        gl.uniform3fv       (spikeProgram.spike_color, vnn.params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        clearExpiredSpikes (time);
        updateSpikesArray ();

        gl.vertexAttribPointer (spikeProgram.position,     POS_SIZE, gl.FLOAT, false, SPIKE_BYTES, SPIKE_POS1_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.end_position, POS_SIZE, gl.FLOAT, false, SPIKE_BYTES, SPIKE_POS2_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.duration,            1, gl.FLOAT, false, SPIKE_BYTES, SPIKE_DURATION_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.end_time,            1, gl.FLOAT, false, SPIKE_BYTES, SPIKE_END_TIME_BYTE_OFFSET);

        gl.drawArrays (gl.POINTS, 0, numSpikes);
    }

    this.drawPicker = function (pMatrix, mvMatrix)
    {
        useProgram (nodePickerProgram, pMatrix, mvMatrix);
        gl.enableVertexAttribArray (nodePickerProgram.id);

        gl.uniform1f (nodePickerProgram.point_size, vnn.params.point_size);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);

        gl.vertexAttribPointer (nodePickerProgram.position, POS_SIZE, gl.FLOAT, false, NODE_BYTES, NODE_POS_BYTE_OFFSET);
        gl.vertexAttribPointer (nodePickerProgram.id,              1, gl.FLOAT, false, NODE_BYTES, NODE_ID_BYTE_OFFSET);

        gl.drawArrays (gl.POINTS, 0, numNodes);
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
            var i = nodeSpikes.pop () * NODE_SIZE + NODE_END_TIME_OFFSET;
            var a = nodesArray.subarray (i, i + 1);
            if (a.length)
                gl.bufferSubData (gl.ARRAY_BUFFER, i * FLOAT_SIZE, a);
        }

    }

    function useProgram (program, pMatrix, mvMatrix)
    {
        gl.useProgram (program);
        gl.enableVertexAttribArray (program.position);

        gl.uniformMatrix4fv (program.pMatrix,  false, pMatrix);
        gl.uniformMatrix4fv (program.mvMatrix, false, mvMatrix);
        gl.uniform1f (program.log_far_const, vnn.params.log_far_const);
        gl.uniform1f (program.far,           vnn.params.far);
        gl.uniform3fv (program.clear_color, vnn.params.clear_color);
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
