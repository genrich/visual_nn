function initController (vnn, canvas, mvMatrix)
{
    const FRAMEBUFFER_RANGE = 511;

    var eye              = vec3.fromValues (1000, 100, 0),
        up               = vec3.fromValues (0, 1, 0),
        center           = vec3.fromValues (0, 0, 0),
        lookOutDirection = vec3.create (),
        right            = vec3.create ();

    var mouseDown = false,
        lastMouseX, lastMouseY,
        mouseDownX, mouseDownY,
        buttonState;

    canvas.oncontextmenu = function (evnt) { evnt.preventDefault (); };
    canvas.onmousedown = handleMouseDown;
    canvas.onwheel     = handleWheel;
    canvas.onclick     = handleClick;
    document.onmouseup   = handleMouseUp;
    document.onmousemove = handleMouseMove;

    mat4.lookAt (mvMatrix, eye, center, up);

    function handleClick (evnt)
    {
        var newX = evnt.clientX;
        var newY = evnt.clientY;
        if (Math.abs (newX - mouseDownX) < 3 && Math.abs (newY - mouseDownY) < 3)
        {
            var x = Math.round (newX                   / canvas.width  * FRAMEBUFFER_RANGE);
            var y = Math.round ((canvas.height - newY) / canvas.height * FRAMEBUFFER_RANGE);
            vnn.pickerClicked.dispatch (x, y);
        }
    }

    function handleMouseDown (evnt)
    {
        evnt.preventDefault ();

        mouseDown = true;
        lastMouseX = evnt.clientX;
        lastMouseY = evnt.clientY;
        mouseDownX = evnt.clientX;
        mouseDownY = evnt.clientY;
        buttonState = evnt.button;
    }

    function handleMouseUp (evnt)
    {
        mouseDown = false;
    }

    function handleMouseMove (evnt)
    {
        var newX = evnt.clientX;
        var newY = evnt.clientY;

        if (mouseDown)
        {
            var deltaX = (newX - lastMouseX) * 5;
            var deltaY = (newY - lastMouseY) * 5;

            vec3.sub (lookOutDirection, eye, center);
            vec3.cross (right, lookOutDirection, up);

            switch (buttonState)
            {
                case 0: // rotate
                    vec3.normalize (right, right);
                    var tmp         = vec3.create (),
                        lookOutNorm = vec3.create (),
                        rot         = quat.create ();
                    vec3.scaleAndAdd (tmp, lookOutDirection, right, deltaX);
                    vec3.scaleAndAdd (tmp, tmp, up, deltaY);

                    vec3.normalize (lookOutNorm, lookOutDirection);
                    vec3.normalize (tmp,         tmp);

                    quat.rotationTo (rot, lookOutNorm, tmp);

                    vec3.transformQuat (lookOutDirection, lookOutDirection, rot);

                    vec3.add (eye, center, lookOutDirection);
                    break;
                case 1: // move
                    vec3.normalize (lookOutDirection, lookOutDirection);
                    vec3.scaleAndAdd (eye,    eye,    lookOutDirection, deltaY);
                    vec3.scaleAndAdd (center, center, lookOutDirection, deltaY);
                    break;
                case 2: // pan
                    vec3.normalize (right, right);
                    vec3.scaleAndAdd (eye, eye, right, deltaX);
                    vec3.scaleAndAdd (eye, eye, up,    deltaY);
                    vec3.scaleAndAdd (center, center, right, deltaX);
                    vec3.scaleAndAdd (center, center, up,    deltaY);
                    break;
            }

            mat4.lookAt (mvMatrix, eye, center, up);

            lastMouseX = newX
            lastMouseY = newY;
        }
        else
        {
            var x = Math.round (newX                   / canvas.width  * FRAMEBUFFER_RANGE);
            var y = Math.round ((canvas.height - newY) / canvas.height * FRAMEBUFFER_RANGE);
            vnn.pickerMoved.dispatch (x, y);
        }
    }

    function handleWheel (evnt)
    {
        evnt.preventDefault ();
        evnt.stopPropagation ();

        var zoom;
        if (evnt.deltaY < 0)
            zoom = 0.95; // zoom in
        else
            zoom = 1.05; // zoom out;

        vec3.sub (lookOutDirection, eye, center);
        vec3.scaleAndAdd (eye, center, lookOutDirection, zoom);
        mat4.lookAt (mvMatrix, eye, center, up);
    }
}
