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
