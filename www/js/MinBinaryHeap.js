function MinBinaryHeap ()
{
    this.pop = function ()
    {
        if (array.length === 0)
            throw 'heap is empty!';

        var value   = array[0],
            lastIdx = array.length - 1;

        array[0] = array[lastIdx];
        array.length = lastIdx;

        if (lastIdx > 0)
            bubbleDown (0);

        return value.index;
    }

    this.push = function (time, index)
    {
        array.push ({time: time, index: index});
        bubbleUp (array.length - 1);
    }

    this.top = function ()
    {
        return array[0].time;
    }

    this.isTopExpired = function (time)
    {
        return array.length > 0 && this.top () < time;
    }

    this.size = function ()
    {
        return array.length;
    }

    var array   = [],
        indexes = [];

    function swap (idx1, idx2)
    {
        var tmp = array[idx1];
        array[idx1] = array[idx2];
        array[idx2] = tmp;
    }

    function bubbleDown (idx)
    {
        var leftIdx     = 2 * idx + 1,
            rightIdx    = leftIdx + 1,
            smallestIdx = idx;
        if (leftIdx < array.length && array[smallestIdx].time > array[leftIdx].time)
            smallestIdx = leftIdx;

        if (rightIdx < array.length && array[smallestIdx].time > array[rightIdx].time)
            smallestIdx = rightIdx;

        if (smallestIdx != idx)
        {
            swap (smallestIdx, idx);
            bubbleDown (smallestIdx);
        }
    }

    function bubbleUp (idx)
    {
        if (idx <= 0)
            return;

        var parentIdx = Math.floor ((idx - 1) / 2);
        if (array[parentIdx].time > array[idx].time)
        {
            swap (parentIdx, idx);
            bubbleUp (parentIdx);
        }
    }
}
