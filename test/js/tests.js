test ("Single segment, no move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 1, iSeg = 0, coefDelta = 0

    connCoef[0] = 1
    connCoef[1] = 0

    processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)

    deepEqual (connCoef, [1, 0])
})

test ("Single segment, 0.25 move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 1, iSeg = 0, coefDelta = 0.25

    connCoef[0] = 1
    connCoef[1] = 0

    processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)

    deepEqual (connCoef, [0.75, 0.25])
})

test ("Single segment, 0.75 move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 1, iSeg = 0, coefDelta = 0.75

    connCoef[0] = 1
    connCoef[1] = 0

    processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)

    deepEqual (connCoef, [0.25, 0.75])
})

test ("Single segment, full move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 1, iSeg = 0, coefDelta = 1

    connCoef[0] = 1
    connCoef[1] = 0

    processSegment (connCoef, startIdx, connSegCount, iSeg, coefDelta)

    deepEqual (connCoef, [0, 1])
})

test ("Two segments, 1.25 move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 2, coefDelta = 1.25

    connCoef[0] = 1
    connCoef[1] = 0
    connCoef[2] = 0
    connCoef[3] = 0

    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0, 0, 0.75, 0.25])
})

test ("Two segments, 1.75 move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 2, coefDelta = 1.75

    connCoef[0] = 1
    connCoef[1] = 0
    connCoef[2] = 0
    connCoef[3] = 0

    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0, 0, 0.25, 0.75])
})

test ("Two segments, full move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 2, coefDelta = 2

    connCoef[0] = 1
    connCoef[1] = 0
    connCoef[2] = 0
    connCoef[3] = 0

    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0, 0, 0, 1])
})

test ("Three segments, full move test", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 3, coefDelta = 3

    connCoef[0] = 1
    connCoef[1] = 0
    connCoef[2] = 0
    connCoef[3] = 0
    connCoef[4] = 0
    connCoef[5] = 0

    processSegment (connCoef, startIdx, connSegCount, 2, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0, 0, 0, 0, 0, 1])
})

test ("Three segments", function()
{
    var connCoef = [], startIdx = 0, connSegCount = 3, coefDelta = 0.5

    connCoef[0] = 0.75
    connCoef[1] = 0.25
    connCoef[2] = 0
    connCoef[3] = 0
    connCoef[4] = 0
    connCoef[5] = 0

    processSegment (connCoef, startIdx, connSegCount, 2, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0.25, 0.75, 0, 0, 0, 0])

    connCoef[0] = 0.75
    connCoef[1] = 0.75
    connCoef[2] = 0
    connCoef[3] = 0
    connCoef[4] = 0
    connCoef[5] = 0

    processSegment (connCoef, startIdx, connSegCount, 2, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 1, coefDelta)
    processSegment (connCoef, startIdx, connSegCount, 0, coefDelta)

    deepEqual (connCoef, [0.25, 0, 0, 0.25, 0, 0])
})
