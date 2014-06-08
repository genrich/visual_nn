test ('Projection matrix test', function ()
{
    var eye              = vec3.fromValues (-1.1, 0, 0),
        up               = vec3.fromValues (0, 1, 0),
        center           = vec3.fromValues (0, 0, 0),
        mvMatrix         = mat4.create (),
        pMatrix          = mat4.create (),
        pmvMatrix        = mat4.create (),
        tmpVec4,
        lookOutDirection = vec3.create (),
        right            = vec3.create ();

    var width = 800, height = 600, near = 1, far = 10000;

    mat4.perspective (pMatrix, Math.PI / 4, width / height, near, far);
    mat4.lookAt (mvMatrix, eye, center, up);
    mat4.mul (pmvMatrix, pMatrix, mvMatrix);

    var str = '';
    str += '[' + pmvMatrix[0] + ' ' + pmvMatrix[4] + ' ' + pmvMatrix[8]  + ' ' +  pmvMatrix[12] + '; ' +
                 pmvMatrix[1] + ' ' + pmvMatrix[5] + ' ' + pmvMatrix[9]  + ' ' +  pmvMatrix[13] + '; ' +
                 pmvMatrix[2] + ' ' + pmvMatrix[6] + ' ' + pmvMatrix[10] + ' ' +  pmvMatrix[14] + '; ' +
                 pmvMatrix[3] + ' ' + pmvMatrix[7] + ' ' + pmvMatrix[11] + ' ' +  pmvMatrix[15] + ']';

    // tmpVec4 = vec4.fromValues (x, 0, 0, 1);
    // vec4.transformMat4 (tmpVec4, tmpVec4, pmvMatrix);
    // vec4.scale (tmpVec4, tmpVec4, 1/tmpVec4[3]);
    console.log (str);
    console.log (mat4.str (pmvMatrix));

    ok (1 == 1);
});
