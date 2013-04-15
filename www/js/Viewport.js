function Viewport (sig)
{
    var viewport = new UI.Panel ({clazz: "Viewport"})

    if (!Detector.webgl) Detector.addGetWebGLMessage ()

    var geometry = new THREE.Geometry ()
    var colors = []
    var count = 0
    sig.wsMsgReceived.add (function (data)
    {
        var x = new Int32Array (data)
        var randomIdx = Math.floor (Math.random () * (count + 1))
        if (data && x[0] == 1) colors[randomIdx].set (0xffffff)
        else                   colors[randomIdx].set (0x000000)
        geometry.colorsNeedUpdate = true
    })

    var sceneHelpers = new THREE.Scene ()

    var step = 20
    var inputGrid = new THREE.GridHelper (200, step)
    inputGrid.position.y = -300
    sceneHelpers.add (inputGrid)

    var outputGrid = new THREE.GridHelper (200, step)
    outputGrid.position.y = 300
    sceneHelpers.add (outputGrid)

    var scene = new THREE.Scene ()
    scene.fog = new THREE.FogExp2 (0x000000, 0.001)
    scene.add (function ()
    {
        var sprite = THREE.ImageUtils.loadTexture ("textures/ball.png")

        for (var y = -300; y <= 300; y += 600)
            for (var x = -10*step + step/2; x < 10*step; x += step)
                for (var z = -10*step + step/2; z < 10*step; z += step)
                {
                    geometry.vertices.push (new THREE.Vector3 (x, y, z))
                    colors[count++] = new THREE.Color (0xffffff)
                }

        for (var i = 0; i < 1000; i++)
        {
            geometry.vertices.push (new THREE.Vector3 (Math.random () * 380 - 190, Math.random () * 580 - 290, Math.random () * 380 - 190))
            colors[count++] = new THREE.Color (0xffffff)
        }

        geometry.colors = colors
        var material = new THREE.ParticleBasicMaterial ({size: 20, map: sprite, vertexColors: true, alphaTest: 0.5})

        return new THREE.ParticleSystem (geometry, material)
    } ())

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

    ;(function animate ()
    {
        requestAnimationFrame (animate)

        controls.update ()

        renderer.clear ()
        renderer.render (scene, camera)
        renderer.render (sceneHelpers, camera)
    }) ()

    return viewport
}
