function Network (gl, vnn)
{
    const MAX_ID = 16777215, FLOAT_SIZE = 4, INT_SIZE = 2,
          // x, y, z
          POS_SIZE = 3,
          // x1, y1, z1, duration1=0, end_time1=spike_time+attenuation, x2, y2, z3, duration2=length/speed, end_time2=end_time1+duration2
          SPIKE_ITEM_SIZE              = 10,
          SPIKE_POS_1_OFFSET           = 0,
          SPIKE_DURATION_1_OFFSET      = 3,
          SPIKE_END_TIME_1_OFFSET      = 4,
          SPIKE_POS_2_OFFSET           = 5,
          SPIKE_DURATION_2_OFFSET      = 8,
          SPIKE_END_TIME_2_OFFSET      = 9,
          SPIKE_BYTES                  = SPIKE_ITEM_SIZE         * FLOAT_SIZE,
          SPIKE_POS_1_BYTE_OFFSET      = SPIKE_POS_1_OFFSET      * FLOAT_SIZE,
          SPIKE_DURATION_1_BYTE_OFFSET = SPIKE_DURATION_1_OFFSET * FLOAT_SIZE,
          SPIKE_END_TIME_1_BYTE_OFFSET = SPIKE_END_TIME_1_OFFSET * FLOAT_SIZE,
          SPIKE_POS_2_BYTE_OFFSET      = SPIKE_POS_2_OFFSET      * FLOAT_SIZE,
          SPIKE_DURATION_2_BYTE_OFFSET = SPIKE_DURATION_2_OFFSET * FLOAT_SIZE,
          SPIKE_END_TIME_2_BYTE_OFFSET = SPIKE_END_TIME_2_OFFSET * FLOAT_SIZE,
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
    var numNodes = 0, nodesArray = new Float32Array (NODE_BUF_INC), nodesToUpdate = [], isNodesBufferFresh = true, nodeSpikesToUpdate = [];
    // adjacency list, adjacencyList[v_idx] = [u_idx : adjacent_vertex (v_idx), e_idx : corresponding_edge_idx (v_idx, u_idx), ...]
    var adjacencyList = [];
    // edgesArray = [v_idx, u_idx, ...]
    var numEdges = 0, edgesArray = new Uint16Array (EDGE_BUF_INC), isEdgesBufferFresh = true;
    // spike array, [pos_start, pos_end, duration, end_time]
    var edgeSpikesPool = new PseudoQueue (), spikesArray = new Float32Array (SPIKE_BUF_INC), numSpikes = 0, radiatingSpikes = [];
    var heap = new MinBinaryHeap ();


    var nodeProgram       = initProgram (gl, 'node'),
        nodePickerProgram = initProgram (gl, 'nodePicker'),
        connectionProgram = initProgram (gl, 'connection'),
        spikeProgram      = initProgram (gl, 'spike'),
        spikeLinesProgram = initProgram (gl, 'spikeLines');

    var nodesBuffer = gl.createBuffer (),
        edgesBuffer = gl.createBuffer (),
        spikeBuffer = gl.createBuffer ();

    nodeProgram.end_time   = gl.getAttribLocation (nodeProgram, 'end_time');
    nodeProgram.attributes = gl.getAttribLocation (nodeProgram, 'attributes');
    nodeProgram.time        = gl.getUniformLocation (nodeProgram, 'time');
    nodeProgram.attenuation = gl.getUniformLocation (nodeProgram, 'attenuation');
    nodeProgram.rest_color  = gl.getUniformLocation (nodeProgram, 'rest_color');
    nodeProgram.spike_color = gl.getUniformLocation (nodeProgram, 'spike_color');
    nodeProgram.point_size  = gl.getUniformLocation (nodeProgram, 'point_size');

    nodePickerProgram.id = gl.getAttribLocation (nodePickerProgram, 'id');
    nodePickerProgram.point_size = gl.getUniformLocation (nodePickerProgram, 'point_size');

    connectionProgram.end_time = gl.getAttribLocation (connectionProgram, 'end_time');
    connectionProgram.duration = gl.getAttribLocation (connectionProgram, 'duration');
    connectionProgram.time        = gl.getUniformLocation (connectionProgram, 'time');
    connectionProgram.spike_color = gl.getUniformLocation (connectionProgram, 'spike_color');
    connectionProgram.rest_color  = gl.getUniformLocation (connectionProgram, 'rest_color');

    spikeProgram.end_position = gl.getAttribLocation (spikeProgram, 'end_position');
    spikeProgram.duration     = gl.getAttribLocation (spikeProgram, 'duration');
    spikeProgram.end_time     = gl.getAttribLocation (spikeProgram, 'end_time');
    spikeProgram.time        = gl.getUniformLocation (spikeProgram, 'time');
    spikeProgram.attenuation = gl.getUniformLocation (spikeProgram, 'attenuation');
    spikeProgram.spike_color = gl.getUniformLocation (spikeProgram, 'spike_color');

    spikeLinesProgram.duration = gl.getAttribLocation (spikeLinesProgram, 'duration');
    spikeLinesProgram.end_time = gl.getAttribLocation (spikeLinesProgram, 'end_time');
    spikeLinesProgram.time        = gl.getUniformLocation (spikeLinesProgram, 'time');
    spikeLinesProgram.attenuation = gl.getUniformLocation (spikeLinesProgram, 'attenuation');
    spikeLinesProgram.rest_color  = gl.getUniformLocation (spikeLinesProgram, 'rest_color');
    spikeLinesProgram.spike_color = gl.getUniformLocation (spikeLinesProgram, 'spike_color');

    gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, nodesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, edgesBuffer);
    gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, edgesArray, gl.DYNAMIC_DRAW);

    gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);
    gl.bufferData (gl.ARRAY_BUFFER, spikesArray, gl.DYNAMIC_DRAW);

    this.clear = function ()
    {
        numNodes = 0; nodesToUpdate = [];
        numSpikes = 0; nodeSpikesToUpdate = [];
        adjacencyList = [];
        numEdges = 0;
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
        nodesArray[i_start + NODE_END_TIME_OFFSET] = - vnn.params.absolute_refractory * vnn.params.slowdown;
        nodesArray[i_start + NODE_ATTR_OFFSET]     = 0;

        nodesToUpdate.push (i);

        if (adjacencyList[i] === undefined) adjacencyList[i] = [];
    }

    var selInbound = [], selOutbound = [], selNeighbours = [],
        selInConnArray  = new Uint16Array (),
        selOutConnArray = new Uint16Array (),
        selInConnBuf  = gl.createBuffer (),
        selOutConnBuf = gl.createBuffer (),
        lastSelectedId = makeRef ();

    this.select = function (id)
    {
        processSelection (lastSelectedId, id,
            function (id)
            {
                vnn.selectNode (id);
            },
            function (id) // called when selection is cleared
            {
                selInConnArray  = new Uint16Array ();
                selOutConnArray = new Uint16Array ();
                var clearAttrFun = function (i) { nodesArray[i * NODE_SIZE + NODE_ATTR_OFFSET] = 0; nodesToUpdate.push (i); }
                selInbound.   forEach (clearAttrFun);
                selOutbound.  forEach (clearAttrFun);
                selNeighbours.forEach (clearAttrFun);
                selInbound    = [];
                selOutbound   = [];
                selNeighbours = [];
            });
    }

    this.selected_neighbour = function (id)
    {
        if (lastSelectedId < MAX_ID)
        {
            selNeighbours.push (id);
            nodesArray[id * NODE_SIZE + NODE_ATTR_OFFSET] = vecToInt (vnn.params.neighbour_color);
            nodesToUpdate.push (id);
        }
    }

    this.selected_inbound = function (id)
    {
        if (lastSelectedId < MAX_ID)
        {
            selInbound.push (id);
            nodesArray[id * NODE_SIZE + NODE_ATTR_OFFSET] = vecToInt (vnn.params.inbound_color);
            nodesToUpdate.push (id);

            var len = selInConnArray.length;
            selInConnArray = reallocateUintArray (selInConnArray, len + 2);
            selInConnArray[len]     = +lastSelectedId;
            selInConnArray[len + 1] = id;

            gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, selInConnBuf);
            gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, selInConnArray, gl.STATIC_DRAW);
        }
    }

    this.selected_outbound = function (id)
    {
        if (lastSelectedId < MAX_ID)
        {
            selOutbound.push (id);
            nodesArray[id * NODE_SIZE + NODE_ATTR_OFFSET] = vecToInt (vnn.params.outbound_color);
            nodesToUpdate.push (id);

            var len = selOutConnArray.length;
            selOutConnArray = reallocateUintArray (selOutConnArray, len + 2);
            selOutConnArray[len]     = +lastSelectedId;
            selOutConnArray[len + 1] = id;

            gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, selOutConnBuf);
            gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, selOutConnArray, gl.STATIC_DRAW);
        }
    }

    var lastHoverId = makeRef ();
    this.hover = function (id)
    {
        processSelection (lastHoverId, id,
                          function (i) { nodesArray[i * NODE_SIZE + NODE_ATTR_OFFSET] = vecToInt (vnn.params.hover_color); },
                          function (i) { nodesArray[i * NODE_SIZE + NODE_ATTR_OFFSET] = 0; });
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
        if (edgesArray.length < (numEdges + 1) * 2)
            edgesArray = reallocateUintArray (edgesArray, (numEdges + 1) * 2 + EDGE_BUF_INC);

        edgesArray[numEdges * 2]     = from;
        edgesArray[numEdges * 2 + 1] = to;

        isEdgesBufferFresh = false;

        prefill (from, to);

        ++numEdges;
    }

    this.spike = function (v_i)
    {
        if (adjacencyList[v_i] === undefined) return;

        var v_idx = v_i * NODE_SIZE;

        var time = performance.now () / 1000;

        if (v_i < numNodes)
        {
            var i = v_idx + NODE_END_TIME_OFFSET;
            nodesArray[i] = time;

            nodeSpikesToUpdate.push (v_i);
        }

        for (var i = 0; i < adjacencyList[v_i].length; i += 2)
        {
            var u_idx     = adjacencyList[v_i][i] * NODE_SIZE,
                end_time  = time + attenuation ();
                duration  = distance (v_idx, u_idx) / vnn.params.spike_speed * vnn.params.slowdown;

            radiatingSpikes.push (nodesArray[v_idx], nodesArray[v_idx + 1], nodesArray[v_idx + 2], 0,        end_time,
                                  nodesArray[u_idx], nodesArray[u_idx + 1], nodesArray[u_idx + 2], duration, end_time + duration);
        }
    }

    this.drawNodes = function (pMatrix, mvMatrix, time)
    {
        useProgram (nodeProgram, pMatrix, mvMatrix);

        gl.enableVertexAttribArray (nodeProgram.end_time);
        gl.enableVertexAttribArray (nodeProgram.attributes);

        gl.uniform1f (nodeProgram.time,        time);
        gl.uniform1f (nodeProgram.attenuation, attenuation ());
        gl.uniform1f (nodeProgram.point_size,  vnn.params.point_size);
        gl.uniform3fv (nodeProgram.rest_color,  vnn.params.rest_color);
        gl.uniform3fv (nodeProgram.spike_color, vnn.params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
        updateNodes ();
        updateNodeSpikesEndTime ();

        gl.vertexAttribPointer (nodeProgram.position,   POS_SIZE, gl.FLOAT, false, NODE_BYTES, NODE_POS_BYTE_OFFSET);
        gl.vertexAttribPointer (nodeProgram.end_time,          1, gl.FLOAT, false, NODE_BYTES, NODE_END_TIME_BYTE_OFFSET);
        gl.vertexAttribPointer (nodeProgram.attributes,        1, gl.FLOAT, false, NODE_BYTES, NODE_ATTR_BYTE_OFFSET);

        gl.drawArrays (gl.POINTS, 0, numNodes);
    }

    this.drawConnections = function (pMatrix, mvMatrix)
    {
        useProgram (connectionProgram, pMatrix, mvMatrix);

        gl.uniform3fv (connectionProgram.rest_color,  vnn.params.connection_color);
        gl.uniform3fv (connectionProgram.spike_color, vnn.params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, nodesBuffer);
        gl.vertexAttribPointer (connectionProgram.position, POS_SIZE, gl.FLOAT, false, NODE_BYTES, NODE_POS_BYTE_OFFSET);

        gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, edgesBuffer);
        updateEdges ();

        gl.drawElements (gl.LINES, numEdges * 2, gl.UNSIGNED_SHORT, 0);

        // selected inbound/outbound
        if (selInConnArray.length)
        {
            gl.uniform3fv (connectionProgram.rest_color, vnn.params.inbound_color);
            gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, selInConnBuf);
            gl.drawElements (gl.LINES, selInConnArray.length, gl.UNSIGNED_SHORT, 0);
        }

        if (selOutConnArray.length)
        {
            gl.uniform3fv (connectionProgram.rest_color, vnn.params.outbound_color);
            gl.bindBuffer (gl.ELEMENT_ARRAY_BUFFER, selOutConnBuf);
            gl.drawElements (gl.LINES, selOutConnArray.length, gl.UNSIGNED_SHORT, 0);
        }
    }

    this.drawSpikes = function (pMatrix, mvMatrix, time) // draw spike propagation
    {
        useProgram (spikeProgram, pMatrix, mvMatrix);

        gl.enableVertexAttribArray (spikeProgram.end_position);
        gl.enableVertexAttribArray (spikeProgram.duration);
        gl.enableVertexAttribArray (spikeProgram.end_time);

        gl.uniform1f (spikeProgram.time,        time);
        gl.uniform1f (spikeProgram.attenuation, attenuation ());
        gl.uniform3fv (spikeProgram.spike_color, vnn.params.spike_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        clearExpiredSpikes (time);
        updateSpikesArray ();

        gl.vertexAttribPointer (spikeProgram.position,     POS_SIZE, gl.FLOAT, false, SPIKE_BYTES, SPIKE_POS_1_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.end_position, POS_SIZE, gl.FLOAT, false, SPIKE_BYTES, SPIKE_POS_2_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.duration,            1, gl.FLOAT, false, SPIKE_BYTES, SPIKE_DURATION_2_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeProgram.end_time,            1, gl.FLOAT, false, SPIKE_BYTES, SPIKE_END_TIME_2_BYTE_OFFSET);

        gl.drawArrays (gl.POINTS, 0, numSpikes);

        // lines
        useProgram (spikeLinesProgram, pMatrix, mvMatrix);

        gl.enableVertexAttribArray (spikeLinesProgram.duration);
        gl.enableVertexAttribArray (spikeLinesProgram.end_time);

        gl.uniform1f (spikeLinesProgram.time,        time);
        gl.uniform1f (spikeLinesProgram.attenuation, attenuation ());
        gl.uniform3fv (spikeLinesProgram.spike_color, vnn.params.spike_color);
        gl.uniform3fv (spikeLinesProgram.rest_color,  vnn.params.rest_color);

        gl.bindBuffer (gl.ARRAY_BUFFER, spikeBuffer);

        gl.vertexAttribPointer (spikeLinesProgram.position, POS_SIZE, gl.FLOAT, false, SPIKE_BYTES / 2, SPIKE_POS_1_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeLinesProgram.duration, 1,        gl.FLOAT, false, SPIKE_BYTES / 2, SPIKE_DURATION_1_BYTE_OFFSET);
        gl.vertexAttribPointer (spikeLinesProgram.end_time, 1,        gl.FLOAT, false, SPIKE_BYTES / 2, SPIKE_END_TIME_1_BYTE_OFFSET);

        gl.drawArrays (gl.LINES, 0, numSpikes * 2);
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

    function updateNodeSpikesEndTime ()
    {
        while (nodeSpikesToUpdate.length)
        {
            var i = nodeSpikesToUpdate.pop () * NODE_SIZE + NODE_END_TIME_OFFSET;
            var a = nodesArray.subarray (i, i + 1);
            if (a.length)
                gl.bufferSubData (gl.ARRAY_BUFFER, i * FLOAT_SIZE, a);
        }

    }

    function updateEdges ()
    {
        if (!isEdgesBufferFresh)
        {
            gl.bufferData (gl.ELEMENT_ARRAY_BUFFER, edgesArray, gl.DYNAMIC_DRAW);
            isEdgesBufferFresh = true;
        }
    }

    function vecToInt (vec)
    {
        return (vec[0] * 255) + (vec[1] * 255) * 256 + (vec[2] * 255) * 65536;
    }

    function distance (i, j)
    {
        var x = nodesArray[j]     - nodesArray[i],
            y = nodesArray[j + 1] - nodesArray[i + 1],
            z = nodesArray[j + 2] - nodesArray[i + 2];
        return Math.sqrt (x*x + y*y + z*z);
    }

    function attenuation ()
    {
        return vnn.params.absolute_refractory * vnn.params.slowdown;
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

    function reallocateFloatArray (oldArray, newLength)
    {
        var newArray = new Float32Array (newLength);
        newArray.set (oldArray);
        return newArray;
    }

    function reallocateUintArray (oldArray, newLength)
    {
        var newArray = new Uint16Array (newLength);
        newArray.set (oldArray);
        return newArray;
    }

    function prefill (id1, id2)
    {
        var max = Math.max (id1, id2);
        for (var id = numNodes; id <= max; ++id)
            this.set (id, 0, 0, 0);
    }

    function makeRef ()
    {
        var value = MAX_ID;
        var fun = function (w)
        {
            value = w;
        };
        fun.valueOf = function ()
        {
            return value;
        };
        return fun;
    }

    function processSelection (lastId, id, doSelection, clearSelection)
    {
        if (id < MAX_ID)
        {
            if (lastId == MAX_ID || lastId != id)
            {
                doSelection (id);
                nodesToUpdate.push (id);
            }
            if (lastId != MAX_ID && lastId != id)
            {
                clearSelection (+lastId);
                nodesToUpdate.push (+lastId);
            }
            lastId (id);
        }
        else if (lastId != MAX_ID)
        {
            clearSelection (+lastId);
            nodesToUpdate.push (+lastId);
            lastId (MAX_ID);
        }
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
