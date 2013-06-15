var CONST =
{
    MsgType
    {
        Stimulus: 0
    }
}

function Viewport (sig)
{
    var viewport = new UI.Panel ({clazz: "Viewport"})

    if (!Detector.webgl) Detector.addGetWebGLMessage ()

    var sprite = THREE.ImageUtils.loadTexture ("textures/ball.png")
    var stimulusMaterial = new THREE.ParticleBasicMaterial ({size: 20, map: sprite, vertexColors: true, alphaTest: 0.5})

    var stimulus = new THREE.ParticleSystem (new THREE.Geometry (), stimulusMaterial)

    var cloneFill = function (array, value, count)
    {
        var new_array = []
        for (var i = 0; i < count; i++)
            if (array[i]) new_array[i] = array[i].clone ()
            else          new_array[i] = value.clone ()
        return new_array
    }
    var reserve = function (id) { return Math.ceil ((id + 1) / 100) * 100 }

    var geometry = new THREE.Geometry ()
    var geometry2 = new THREE.Geometry ()
    var colors = []
    var count = 0

    sig.wsMsgReceived.add (function (buffer)
    {
        var type = new Uint32Array (buffer, 0, 1) [0]

        if (type == CONST.MsgType.Stimulus)
        {
            var id = new Uint32Array (buffer, 4, 1) [0]
            var x = new Float32Array (buffer, 8,  1) [0]
            var y = new Float32Array (buffer, 12, 1) [0]
            var z = new Float32Array (buffer, 16, 1) [0]

            var geom = stimulus.geometry
            if (geom.vertices.length <= id)
            {
                geom = new THREE.Geometry ()
                geom.vertices = cloneFill (stimulus.geometry.vertices, new THREE.Vector3 (0, 0, 10000), reserve (id))
                geom.colors   = cloneFill (stimulus.geometry.colors,   new THREE.Color (),              reserve (id))

                scene.remove (stimulus)
                stimulus = new THREE.ParticleSystem (geom, stimulusMaterial)
                scene.add (stimulus)
            }

            geom.vertices[id] = new THREE.Vector3 (x, y, z)
            geom.colors[id] = new THREE.Color (0xffffff)
            geom.verticesNeedUpdate = true
            geom.colorsNeedUpdate   = true
        }
    })

    var sceneHelpers = new THREE.Scene ()

    var step = 20

    var outputGrid = new THREE.GridHelper (200, step)
    outputGrid.position.y = 300
    sceneHelpers.add (outputGrid)

    var scene = new THREE.Scene ()
    scene.fog = new THREE.FogExp2 (0x000000, 0.001)

    scene.add (stimulus)

    scene.add (function ()
    {
        for (var i = 0; i < 100; i++)
        {
            var vertex = new THREE.Vector3 (Math.random () * 380 - 190, Math.random () * 580 - 290, Math.random () * 380 - 190)
            geometry.vertices.push (vertex)
            geometry2.vertices.push (vertex)
            colors[count++] = new THREE.Color (0xffffff)
        }

        geometry.colors = colors
        var material = new THREE.ParticleBasicMaterial ({size: 20, map: sprite, vertexColors: true, alphaTest: 0.5})

        return new THREE.ParticleSystem (geometry, material)
    } ())

    scene.add (function ()
    {
        var lineMaterial = new THREE.LineBasicMaterial ({color: 0x000000, linewidth: 1})
        return new THREE.Line (geometry2, lineMaterial, THREE.LinePieces)
    } ())

    // test segment
    var segmentGeometry = new THREE.Geometry ()
    var white = new THREE.Color (0xfff92e)
    segmentGeometry.vertices.push (new THREE.Vector3 (300, 0, 0))
    segmentGeometry.vertices.push (new THREE.Vector3 (300, 100, 0))
    segmentGeometry.vertices.push (new THREE.Vector3 (300, 100, 0))
    segmentGeometry.vertices.push (new THREE.Vector3 (300, 200, 0))
    segmentGeometry.colors.push (new THREE.Color (0x000000))
    segmentGeometry.colors.push (new THREE.Color (0x000000))
    segmentGeometry.colors.push (new THREE.Color (0x000000))
    segmentGeometry.colors.push (new THREE.Color (0x000000))
    var lineMaterial = new THREE.LineBasicMaterial({vertexColors: THREE.VertexColors})
    var lineSegment = new THREE.Line (segmentGeometry, lineMaterial, THREE.LinePieces)
    scene.add (lineSegment)

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

    ;(function animate (timestamp)
    {
        requestAnimationFrame (animate)

        if (timestamp)
        {
            segmentGeometry.colors[0].copy (white.clone ().multiplyScalar ((Math.sin (timestamp / 500) + 1) / 2))
            segmentGeometry.colors[1].copy (white.clone ().multiplyScalar ((Math.sin (timestamp / 500 + Math.PI/2) + 1) / 2))
            segmentGeometry.colors[2].copy (white.clone ().multiplyScalar ((Math.sin (timestamp / 500 + Math.PI/2) + 1) / 2))
            segmentGeometry.colors[3].copy (white.clone ().multiplyScalar ((Math.sin (timestamp / 500 + Math.PI) + 1) / 2))
            segmentGeometry.colorsNeedUpdate = true
        }

        controls.update ()

        renderer.clear ()
        renderer.render (scene, camera)
        renderer.render (sceneHelpers, camera)
    }) ()

    return viewport
}
