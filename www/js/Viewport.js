function Viewport (sig)
{
    var viewport = new UI.Panel ({clazz: "Viewport"})

    var objects = []
    objects[0] = new THREE.Mesh (new THREE.CubeGeometry (70, 70, 70), new THREE.MeshBasicMaterial ({color: 0xffffff}))
    sig.wsMsgReceived.add (function (data)
    {
        var x = new Int32Array (data)
        if (data && x[0] == 1) objects[0].material.color.set (0xffffff)
        else                   objects[0].material.color.set (0x000000)
    })

    var sceneHelpers = new THREE.Scene ()

    var grid = new THREE.GridHelper (500, 25)
    sceneHelpers.add (grid)

    var scene = new THREE.Scene ()
    scene.add (objects[0])

    var camera = new THREE.PerspectiveCamera (50, viewport.dom.offsetWidth / viewport.dom.offsetHeight, 1, 5000)
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

        sceneHelpers.updateMatrixWorld ()
        scene.updateMatrixWorld ()

        renderer.clear ()
        renderer.render (scene, camera)
        renderer.render (sceneHelpers, camera)
    }) ()

    return viewport
}
