/**
 * veda.util.container
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

module veda.util.container;

import std.stdio, std.conv;

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

////////////////////////////////////////////////////////

struct CacheElement (T, K)
{
    private T data;
    private K key;
    long      use_count = 0;
    long      use_time  = 0;
    long      MRU_pos   = 0;
}

class Cache(T, K)
{
    long                    max_size;
    private                 CacheElement!(T, K) *[ K ] key_2_element;
    string                  id;

    CacheElement!(T, K) *[] MRU;

    this(long _max_size, string _id)
    {
        max_size   = _max_size;
        MRU.length = max_size;
        id         = _id;
    }

    public void printMRU()
    {
        string ss;

        foreach (MRU_e; MRU)
        {
            if (MRU_e !is null)
            {
                ss ~= "[" ~ text(MRU_e.key) ~ ":" ~ text(MRU_e.use_count) ~ ":" ~ text(MRU_e.MRU_pos) ~ "]";
            }
        }
        writeln("\n\n@MRU=", ss);
    }

    public void put(K, T) (K key, T src, int level = 0)
    {
        if (key_2_element.length < max_size)
        {
            CacheElement!(T, K) * ce = new CacheElement!(T, K)();
            ce.data                  = src;
            ce.MRU_pos               = key_2_element.length;
            ce.key                   = key;

            MRU[ ce.MRU_pos ]    = ce;
            key_2_element[ key ] = ce;
        }
        else
        {
            //printMRU();
            //writeln("key=", key, ", id=", id, ", MAX SIZE=", key_2_element.length);
            // найдем самый старый и малоиспользуемый элемент
            // удалим его

            long pp = key_2_element.length - 1;
            while (pp > 0)
            {
                //writeln ("level=", level, ", pp=", pp);
                CacheElement!(T, K) * ce = MRU[ pp ];

                if (ce !is null)
                {
                    key_2_element.remove(ce.key);
                    put(key, src, level + 1);
                    return;
                }

                pp--;
            }
        }
    }

    public T get(K) (K key)
    {
        //writeln("%read ", key);
        CacheElement!(T, K) * element;

        element = key_2_element.get(key, null);
        if (element !is null)
        {
            if (element.MRU_pos > 0)
            {
                long cur_pos  = element.MRU_pos;
                long prev_pos = cur_pos - 1;
                //writeln ("@cur_pos=", cur_pos, ", prev_pos=", prev_pos);
                CacheElement!(T, K) * prev_element = MRU[ prev_pos ];

                // проверим предыдущий элемент и если его частота использования меньше нашей, то поменяемся местами
                if (prev_element.use_count <= element.use_count)
                {
                    MRU[ prev_pos ]      = element;
                    element.MRU_pos      = prev_pos;
                    MRU[ cur_pos ]       = prev_element;
                    prev_element.MRU_pos = cur_pos;
                }
            }
            element.use_count++;
            return element.data;
        }
        else
            return T.init;
    }
}

unittest
{
    auto   cache = new Cache!(string, string)(3, "id1");
    string dx;

    cache.put("k1", "d1");
    dx = cache.get!string("k1");
    assert(dx == "d1");

    cache.put("k2", "d2");
    dx = cache.get!string("k2");
    assert(dx == "d2");

    dx = cache.get!string("k2");
    dx = cache.get!string("k2");

    cache.put("k3", "d3");
    dx = cache.get!string("k3");
    dx = cache.get!string("k3");
    dx = cache.get!string("k3");
    dx = cache.get!string("k3");

    dx = cache.get!string("k2");
    dx = cache.get!string("k2");
    dx = cache.get!string("k2");
    dx = cache.get!string("k2");

    cache.put("k4", "d4");
    dx = cache.get!string("k1");
    assert(dx != "d1");
}


public class stack(T)
{
    T[] data;
    int pos;

    this()
    {
        data = new T[ 100 ];
        pos  = 0;
    }

    T back()
    {
        //		writeln("stack:back:pos=", pos, ", data=", data[pos]);
        return data[ pos ];
    }

    T popBack()
    {
        if (pos > 0)
        {
            //			writeln("stack:popBack:pos=", pos, ", data=", data[pos]);
            pos--;
            return data[ pos + 1 ];
        }
        return data[ pos ];
    }

    void pushBack(T val)
    {
        //		writeln("stack:pushBack:pos=", pos, ", val=", val);
        pos++;
        data[ pos ] = val;
    }

    bool empty()
    {
        return pos == 0;
    }
}
