function Viewport (sig)
{
    var COLOR_REST = new THREE.Color (0x555555), COLOR_SPIKE = new THREE.Color (0xfff92e)
    var COLOR_RED = new THREE.Color (0xff0000)
    var DUMMY_POINT = new THREE.Vector3 (0, 0, 10000)
    var stimulusColorAttenuation = 0.5
    var spikeSpeed = 50

    var viewport = new UI.Panel ({clazz: "Viewport"})

    if (!Detector.webgl) Detector.addGetWebGLMessage ()

    var sprite = THREE.ImageUtils.loadTexture ("textures/ball.png")
    var stimulusMaterial = new THREE.ParticleBasicMaterial ({size: 20, map: sprite, vertexColors: true, alphaTest: 0.5})
    var somaMaterial = new THREE.ParticleBasicMaterial ({size: 10, map: sprite, vertexColors: true, alphaTest: 0.5})

    var stimuli = new THREE.ParticleSystem (new THREE.Geometry (), stimulusMaterial)
    var neurons = new THREE.ParticleSystem (new THREE.Geometry (), somaMaterial)
    var connections = new THREE.Line (new THREE.Geometry (),
                                      new THREE.LineBasicMaterial ({vertexColors: THREE.VertexColors}),
                                      THREE.LinePieces)

    var connIdMap = []
    var connLastIdx = 0
    var connSegLengths = []
    var connSegCounts = []
    var connCoef = []

    var newZeroArray = function (count)
    {
        var new_array = []
        for (var i = 0; i < count; i++)
            new_array[i] = 0
        return new_array
    }
    var cloneFill = function (array, value, count)
    {
        var new_array = []
        for (var i = 0; i < count; i++)
            if (array[i]) new_array[i] = array[i].clone ()
            else          new_array[i] = value.clone ()
        return new_array
    }
    var reserve = function (id) { return Math.ceil ((id + 1) / 100) * 100 }

    sig.wsMsgReceived.add (function (buffer)
    {
        var type = new Uint32Array (buffer, 0, 1) [0]

        if (type == CONST.STIMULUS_POS)
        {
            var id = new Uint32Array (buffer, 4, 1) [0]
            var pos = new Float32Array (buffer, 8,  3)

            var geom = stimuli.geometry
            if (geom.vertices.length <= id)
            {
                geom = new THREE.Geometry ()
                geom.vertices = cloneFill (stimuli.geometry.vertices, DUMMY_POINT, reserve (id))
                geom.colors   = cloneFill (stimuli.geometry.colors,   COLOR_REST,  reserve (id))

                scene.remove (stimuli)
                stimuli = new THREE.ParticleSystem (geom, stimulusMaterial)
                scene.add (stimuli)
            }

            geom.vertices[id].set (pos[0], pos[1], pos[2])
            geom.colors[id].copy (COLOR_REST)
            geom.verticesNeedUpdate = true
            geom.colorsNeedUpdate   = true
        }
        else if (type == CONST.STIMULUS_SPIKE)
        {
            var ints = new Uint32Array (buffer, 4, 2)
            var id = ints[0]
            var connCount = ints[1]

            var ints = new Uint32Array (buffer, 12, connCount)
            for (var i = 0; i < connCount; i++)
            {
                var connId = ints[i]
                connCoef[connIdMap[connId]] = 1
                connections.geometry.colors[connIdMap[connId]].copy (COLOR_SPIKE)
                connections.geometry.colorsNeedUpdate = true
            }

            stimuli.geometry.colors[id].copy (COLOR_SPIKE)
            stimuli.geometry.colorsNeedUpdate = true
        }
        else if (type == CONST.SOMA_POS)
        {
            var id = new Uint32Array (buffer, 4, 1) [0]
            var pos = new Float32Array (buffer, 8, 3)

            var geom = neurons.geometry
            if (geom.vertices.length <= id)
            {
                geom = new THREE.Geometry ()
                geom.vertices = cloneFill (neurons.geometry.vertices, DUMMY_POINT, reserve (id))
                geom.colors   = cloneFill (neurons.geometry.colors,   COLOR_REST,  reserve (id))

                scene.remove (neurons)
                neurons = new THREE.ParticleSystem (geom, somaMaterial)
                scene.add (neurons)
            }

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

    var sceneHelpers = new THREE.Scene ()
    var step = 20
    var outputGrid = new THREE.GridHelper (200, step)
    outputGrid.position.y = 300
    sceneHelpers.add (outputGrid)

    var scene = new THREE.Scene ()
    scene.fog = new THREE.FogExp2 (0x000000, 0.0007)

    scene.add (stimuli)
    scene.add (neurons)
    scene.add (connections)

    var camera = new THREE.PerspectiveCamera (70, viewport.dom.offsetWidth / viewport.dom.offsetHeight, 1, 5000)
    camera.position.set (500, 250, 500)
    camera.lookAt (scene.position)

    var controls = new THREE.TrackballControls (camera, viewport.dom)

    sig.windowResized.add (function ()
    {
        camera.aspect = viewport.dom.offsetWidth / viewport.dom.offsetHeight
        camera.updateProjectionMatrix ()

        renderer.setSize (viewport.dom.offsetWidth, viewport.dom.offsetHeight)
    })

    var renderer = new THREE.WebGLRenderer ()
    renderer.autoClear = false
    viewport.dom.appendChild (renderer.domElement)

    var lerp = function (color, delta)
    {
        if (COLOR_REST.r !== Math.round (color.r) || COLOR_REST.g !== Math.round (color.g) || COLOR_REST.b !== Math.round (color.b))
        {
            color.r -= (color.r - COLOR_REST.r) * delta;
            color.g -= (color.g - COLOR_REST.g) * delta;
            color.b -= (color.b - COLOR_REST.b) * delta;
        }
        else
        {
            color.copy (COLOR_REST)
        }
    }

    var lerp2 = function (color, delta)
    {
        color.copy (COLOR_REST)
        if (delta != 0)
        {
            color.r += (COLOR_SPIKE.r - COLOR_REST.r) * delta;
            color.g += (COLOR_SPIKE.g - COLOR_REST.g) * delta;
            color.b += (COLOR_SPIKE.b - COLOR_REST.b) * delta;
        }
    }

    var prevTimestamp = 0
    ;(function animate (timestamp)
    {
        if (timestamp)
        {
            var delta = (timestamp - prevTimestamp) / 1000
            prevTimestamp = timestamp
            if (delta > 1) delta = 0

            var colors = stimuli.geometry.colors
            for (var i = 0; i < colors.length; i++)
                lerp (colors[i], delta * stimulusColorAttenuation)
            stimuli.geometry.colorsNeedUpdate = true

            var colors = connections.geometry.colors
            for (var connId = 0; connId < connIdMap.length; connId++)
            {
                var startIdx = connIdMap[connId]
                if (!startIdx)
                    continue
                var coefDelta = (spikeSpeed * delta) / connSegLengths[connId]

                var connSegCount = connSegCounts[connId]

                for (var iSeg = connSegCounts[connId] - 1; iSeg >= 0; iSeg--)
                    processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)

                var iSeg = 0
                var idx = startIdx + iSeg * 2
                lerp2 (colors[idx],     connCoef[idx])
                lerp2 (colors[idx + 1], Math.max (connCoef[idx + 1], connCoef[idx + 2]))
                for (iSeg = 1; iSeg < connSegCounts[connId] - 1; iSeg++)
                {
                    idx = startIdx + iSeg * 2
                    lerp2 (colors[idx],     Math.max (connCoef[idx - 1], connCoef[idx]))
                    lerp2 (colors[idx + 1], Math.max (connCoef[idx + 1], connCoef[idx + 2]))
                }
                idx = startIdx + iSeg * 2
                lerp2 (colors[idx],     Math.max (connCoef[idx - 1], connCoef[idx]))
                lerp2 (colors[idx + 1], connCoef[idx + 1])
            }
            connections.geometry.colorsNeedUpdate = true
        }

        requestAnimationFrame (animate)

        controls.update ()

        renderer.clear ()
        renderer.render (scene, camera)
        renderer.render (sceneHelpers, camera)
    }) ()

    return viewport
}

function processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)
{
    var coefA = connCoef[startIdx + iSeg * 2]
    var coefB = connCoef[startIdx + iSeg * 2 + 1]

    if (coefA == 0 && coefB == 0) return

    var distFromA = coefA != 0 ? 1 - coefA + coefDelta : (coefB != 0 ?  1 - coefB + coefDelta : 0)
    var distFromB = coefB != 0 ? coefB - 1 + coefDelta : (coefA != 0 ?     -coefA + coefDelta : 0)

    if (1 <= distFromA)
    {
        connCoef[startIdx + iSeg * 2] = 0

        var iSegA = iSeg + distFromA >> 0
        if (iSegA < connSegCount) connCoef[startIdx + iSegA * 2] = 1 - distFromA % 1
    }
    else
    {
        connCoef[startIdx + iSeg * 2] = 1 - distFromA % 1
    }

    if (0 < distFromB)
    {
        connCoef[startIdx + iSeg * 2 + 1] = 0

        if (distFromB % 1)
        {
            var iSegB = iSeg + 1 + distFromB >> 0
            if (iSegB < connSegCount) connCoef[startIdx + iSegB * 2 + 1] = distFromB % 1
        }
        else
        {
            var iSegB = iSeg + distFromB
            if (iSegB < connSegCount) connCoef[startIdx + iSegB * 2 + 1] = 1
        }
    }
    else
    {
        connCoef[startIdx + iSeg * 2 + 1] = distFromB == -1 ? 0 : 1 + distFromB
    }
}
