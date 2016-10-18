module veda.bootstrap;

import std.string, std.process, std.stdio, std.conv, core.sys.posix.signal;

struct ProcessInfo
{
    int    pid;
    string command;
}

extern (C) void handleTermination(int _signal)
{
    writefln("!SYS: VEDA, caught signal: %s", text(_signal));
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}


private int[][ string ] get_processes_info(string[] command_patterns, ref ProcessInfo[ int ] processes)
{
    int[][ string ] command_2_pid;

    long[ string ] keys;

    auto ps = executeShell("ps aux");
    if (ps.status == 0)
    {
        string[] processes_list = ps.output.splitLines;

        if (processes_list.length > 1)
        {
            string[] _keys = processes_list[ 0 ].split();

            foreach (idx, key; _keys)
                keys[ key ] = idx;
        }

        long COMMAND = keys[ "COMMAND" ];
        long PID     = keys[ "PID" ];

        foreach (process_str; processes_list[ 1..$ ])
        {
            string[] _data = process_str.split();

            if (_data.length >= keys.length)
            {
                try
                {
                    foreach (pt; command_patterns)
                    {
                        if (_data[ COMMAND ] == pt || _data[ COMMAND ] == "./" ~ pt)
                        {
                            ProcessInfo pi = ProcessInfo(to!int (_data[ PID ]), _data[ COMMAND ]);
                            processes[ pi.pid ] = pi;

                            int[] pids;
                            pids = command_2_pid.get(pt, pids.init);
                            pids ~= pi.pid;
                            command_2_pid[ pt ] = pids;
                        }
                    }
                }
                catch (Throwable tr)
                {
                    writeln("ERR! ", tr.msg);
                }
            }
        }
    }
    return command_2_pid;
}

void main(char[][] args)
{
    string[ string ] env;
    int      exit_code;

    string[] modules = [ "veda", "veda-webserver", "veda-server", "veda-ttlreader", "veda-fanout-email", "veda-fanout-sql", "veda-scripts", "veda-ft-indexer", "veda-ltr-scripts" ];
    int[][ string ] command_2_pid;

    bool is_found_modules = false;

    for (int attempt = 0; attempt < 10; attempt++)
    {
        is_found_modules = false;
        ProcessInfo[ int ] processes;

        command_2_pid = get_processes_info(modules, processes);

        foreach (ml; modules)
        {
            int[] pids;
            pids = command_2_pid.get(ml, null);
            if (pids !is null)
            {
                foreach (pid; pids)
                {
                    if (pid != thisProcessID())
                    {
                        is_found_modules = true;
                        kill(pid, SIGKILL);
                        writeln("found running module ", ml, " (", pid, "), kill him.");
                    }
                }
            }
        }

        if (is_found_modules == false)
            break;
    }

    if (is_found_modules == true)
    {
        writeln("запуск системы невозможен, в памяти остались модули от предыдущего запуска : ", command_2_pid,
                ", завершите их и повторите попытку запуска.");
        return;
    }

    Pid server_pid;

    foreach (ml; modules)
    {
        if (ml != "veda")
        {
            auto _logFile = File(ml ~ "-errors.log", "w");
            writeln("start " ~ ml);
            auto _pid = spawnProcess("./" ~ ml,
                                     std.stdio.stdin,
                                     std.stdio.stdout,
                                     _logFile, env, Config.suppressConsole);

            if (ml == "veda-server")
            {
                server_pid = _pid;
            }
        }
    }

    exit_code = wait(server_pid);

    if (exit_code == -SIGKILL)
        writeln("veda-server terminated, code=", exit_code);
}
