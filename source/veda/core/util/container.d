/**
 * util.container
 *
 * License:
 *   This Source Code Form is subject to the terms of
 *   the Mozilla Public License, v. 2.0. If a copy of
 *   the MPL was not distributed with this file, You
 *   can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Authors:
 *   Vladimir Panteleev <vladimir@thecybershadow.net>
 */

module util.container;

import std.stdio;

/// Unordered array with O(1) insertion and removal
struct Set (T, uint INITSIZE = 64)
{
    private T[] data;
    size_t      size;

    void resize(int size)
    {
        data.length = size;
    }

    void opOpAssign(string OP) (T item)
    if (OP == "~")
    {
        if (data.length == size)
            data.length = size ? size * 2 : INITSIZE;
        data[ size++ ] = item;
    }

    void opOpAssign(string OP) (Set!(T) _items)
    if (OP == "~")
    {
        if (data.length <= size + _items.length)
            data.length += _items.length + 1;

        foreach (item; _items)
            data[ size++ ] = item;
    }

    int opApply(int delegate(ref T) dg)
    {
        int result = 0;

        for (int i = 0; i < size; i++)
        {
            result = dg(data[ i ]);
            if (result)
                break;
        }
        return result;
    }

    void remove(size_t index)
    {
        assert(index < size);
        data[ index ] = data[ --size ];
    }

    @property T[] items()
    {
        return data[ 0..size ];
    }

    void empty()
    {
        size = 0;
    }

    @property size_t length()
    {
        return size;
    }
}

unittest
{
    Set!int s;
    s ~= 1;
    s ~= 2;
    s ~= 3;
    assert(s.items ==[ 1, 2, 3 ]);
    s.remove(1);
    assert(s.items ==[ 1, 3 ]);
}

