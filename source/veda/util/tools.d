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
