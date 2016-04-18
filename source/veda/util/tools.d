module veda.util.tools;

public ushort ushort_from_buff(ubyte[] buff, int pos)
{
    ushort res = *((cast(ushort *)(buff.ptr + pos)));

    return res;
}

public uint uint_from_buff(ubyte[] buff, int pos)
{
    uint res = *((cast(uint *)(buff.ptr + pos)));

    return res;
}

public ulong ulong_from_buff(ubyte[] buff, int pos)
{
    ulong res = *((cast(ulong *)(buff.ptr + pos)));

    return res;
}

public void uint_to_buff(ubyte[] _buff, int pos, ulong data)
{
    _buff[ pos + 0 ] = (data & 0x000000FF);
    _buff[ pos + 1 ] = (data & 0x0000FF00) >> 8;
    _buff[ pos + 2 ] = (data & 0x00FF0000) >> 16;
    _buff[ pos + 3 ] = (data & 0xFF000000) >> 24;
}

public void ulong_to_buff(ubyte[] _buff, int pos, ulong data)
{
    _buff[ pos + 0 ] = (data & 0x00000000000000FF);
    _buff[ pos + 1 ] = (data & 0x000000000000FF00) >> 8;
    _buff[ pos + 2 ] = (data & 0x0000000000FF0000) >> 16;
    _buff[ pos + 3 ] = (data & 0x00000000FF000000) >> 24;
    _buff[ pos + 4 ] = (data & 0x000000FF00000000) >> 32;
    _buff[ pos + 5 ] = (data & 0x0000FF0000000000) >> 40;
    _buff[ pos + 6 ] = (data & 0x00FF000000000000) >> 48;
    _buff[ pos + 7 ] = (data & 0xFF00000000000000) >> 56;
}

import veda.type, std.stdio;

ubyte[] buff = new ubyte[ 512 ];

struct IndividualsModifyMessage
{
    string new_state;
    string prev_state;
    CMD    cmd;
    bool   is_ok = false;

    this(string data)
    {
        try
        {
            ubyte[] bdata = cast(ubyte[])data;
            //writeln("new IndividualsModifyMessage: ", data);
            cmd = cast(CMD)data[ 0 ];
            uint length1 = uint_from_buff(bdata, 1);
            uint length2 = uint_from_buff(bdata, 5);

            //writeln("IndividualsModifyMessage: cmd=", cmd);
            //writeln("IndividualsModifyMessage: length1=", length1);
            //writeln("IndividualsModifyMessage: length2=", length2);
            int start_m_pos = 9;

            if (length1 + length2 == bdata.length - start_m_pos)
            {
                //writeln ("%5 length1=", length1);
                int b_pos = start_m_pos;
                int e_pos = b_pos + length1;
                new_state = cast(string)bdata[ b_pos..e_pos ].dup;
                //writeln ("%5.1 new_state.length=", new_state.length);
                //writeln("IndividualsModifyMessage: new_state=", new_state);
                //writeln ("%6 length2=", length2);
                b_pos = start_m_pos + length1;
                e_pos = b_pos + length2;
                if (length2 > 0 && b_pos > e_pos)
                    prev_state = cast(string)bdata[ b_pos..e_pos ].dup;

                //writeln ("%6.1 prev_state.length=", prev_state.length);
                //writeln("IndividualsModifyMessage: prev_state=", prev_state);
                //writeln ("%7");
                is_ok = true;
            }
            else
            {
                writeln("ERR!  length1 + length2 (", length1 + length2, ") != data.length - 9 (", data.length - 9, ")");
            }
        }
        catch (Throwable tr)
        {
            writeln("ERR! ex:", tr.msg);
            is_ok = false;
        }
        //writeln ("%e");
    }

    string serialize()
    {
        buff[ 0 ] = cmd;
        uint_to_buff(buff, 1, (cast(ubyte[])new_state).length);
        uint_to_buff(buff, 5, (cast(ubyte[])prev_state).length);

        return cast(string)buff[ 0..9 ] ~new_state ~ prev_state;
    }
}

