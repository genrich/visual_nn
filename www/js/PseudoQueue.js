function PseudoQueue () // fifo is not maintained at early array growth
{
    this.enqueue = function (value)
    {
        var nextTailIdx = inc (tailIdx);

        if (nextTailIdx != headIdx || is_empty) // not full
        {
            array[nextTailIdx] = value;
            tailIdx = nextTailIdx;
        }
        else
        {
            array.push (value);
        }

        is_empty = false;
    }

    this.dequeue = function ()
    {
        if (!is_empty)
        {
            var value = array[headIdx];
            headIdx = inc (headIdx);

            if (inc (tailIdx) == headIdx)
                is_empty = true;

            return value;
        }
        else
        {
            throw 'queue is empty!';
        }
    }

    this.isEmpty = function ()
    {
        return is_empty;
    }

    this.length = function ()
    {
        return array.length;
    }

    this.size = function ()
    {
        if (is_empty)
        {
            return 0;
        }
        else
        {
            if (headIdx <= tailIdx)
                return tailIdx - headIdx + 1;
            else
                return array.length - headIdx + tailIdx + 1;
        }
    }

    function inc (value)
    {
        if (++value < array.length)
            return value;
        else
            return 0;
    }

    var array = [0, 0], headIdx = 1, tailIdx = 0, is_empty = true;
}
