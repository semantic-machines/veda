/**
 * настройка логгирования
 */
module veda.core.common.log_msg;

private import veda.common.logger;

byte[ 1000 ] trace_msg;

// last id = 500
const static long T_REST_1 = 500;
const static long T_ACL_1  = 111;
const static long T_ACL_2  = 112;
const static long T_ACL_3  = 113;

enum : int
{
    T_API_10  = 601,
    T_API_20  = 602,
    T_API_30  = 603,
    T_API_40  = 604,
    T_API_50  = 605,
    T_API_60  = 606,
    T_API_70  = 607,
    T_API_80  = 608,
    T_API_90  = 609,
    T_API_100 = 610,
    T_API_110 = 611,
    T_API_120 = 612,
    T_API_130 = 613,
    T_API_140 = 614,
    T_API_150 = 615,
    T_API_160 = 616,
    T_API_170 = 617,
    T_API_180 = 618,
    T_API_190 = 619,
    T_API_200 = 620,
    T_API_210 = 621,
    T_API_220 = 622,
    T_API_230 = 623,
    T_API_240 = 624
}

int[] T_API_L1 =
[
    T_API_50, T_API_60, T_API_70, T_API_130, T_API_140, T_API_150, T_API_160, T_API_170, T_API_180,
    T_API_190, T_API_200, T_API_210, T_API_230, T_API_240
];

static this()
{
    trace_msg      = 0;
    trace_msg[ 2 ] = 0;
    trace_msg[ 3 ] = 0;

    // basis logging
    trace_msg[ 0 ]  = 1;
    trace_msg[ 10 ] = 1;
    trace_msg[ 68 ] = 1;
    trace_msg[ 69 ] = 1;

    version (trace)
        trace_msg[] = 1;     // полное логгирование

    version (trace_acl)
    {
        trace_msg[ T_ACL_1 ] = 1;
        trace_msg[ T_ACL_2 ] = 1;
        trace_msg[ T_ACL_3 ] = 1;
    }
    version (no_trace_acl)
    {
        trace_msg[ T_ACL_1 ] = 0;
        trace_msg[ T_ACL_2 ] = 0;
        trace_msg[ T_ACL_3 ] = 0;
    }
    version (trace_api_l1)
    {
        foreach (idx; T_API_L1)
            trace_msg[ idx ] = 1;
    }
}

void set_message(int idx)
{
    trace_msg[ idx ] = 1;
}

void set_all_messages()
{
    trace_msg = 1;
}

void unset_all_messages()
{
    trace_msg = 0;
}

void unset_message(int idx)
{
    trace_msg[ idx ] = 0;
}


void set_trace(int idx, bool state)
{
    if (idx < 0 || idx >= trace_msg.length)
        return;

    if (idx == 0)
        trace_msg = state;
    else
        trace_msg[ idx ] = state;
}
