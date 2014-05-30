function MouseController (canvas, mvMatrix)
{
    var mouseDown  = false;
    var lastMouseX = null;
    var lastMouseY = null;

    function handleMouseDown (event)
    {
        mouseDown = true;
        lastMouseX = event.clientX;
        lastMouseY = event.clientY;
    }

    function handleMouseUp (event)
    {
        mouseDown = false;
    }

    function degToRad (degrees)
    {
        return degrees * Math.PI / 180;
    }

    function handleMouseMove (event)
    {
        if (mouseDown)
        {
            var newX = event.clientX;
            var newY = event.clientY;

            var deltaX = (newX - lastMouseX) * 5;
            var rot = mat4.create ();
            mat4.identity (rot);
            mat4.rotate (rot, rot, degToRad (deltaX / 10), [0, 1, 0]);

            var deltaY = (newY - lastMouseY) * 5;
            mat4.rotate (rot, rot, degToRad (deltaY / 10), [1, 0, 0]);

            mat4.multiply (mvMatrix, mvMatrix, rot);

            lastMouseX = newX
            lastMouseY = newY;
        }
        else
        {
        }
    }

    canvas.onmousedown   = handleMouseDown;
    document.onmouseup   = handleMouseUp;
    document.onmousemove = handleMouseMove;
}
