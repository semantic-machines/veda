module veda.bootstrap;

import std.string, std.process, std.stdio, std.conv, core.sys.posix.signal, std.file;
import commando;

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

import veda.util.module_info;
import veda.util.queue;

bool kill_prev_instance(ref string[] modules, ref int[][ string ] command_2_pid)
{
    bool is_found_modules;

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

    return is_found_modules;
}

void main(string[] args)
{
    bool need_remove_ontology = false;
    bool need_reload_ontology = false;

    foreach (arg; args)
    {
        if (arg == "remove-ontology")
            need_remove_ontology = true;
        if (arg == "reload-ontology")
            need_reload_ontology = true;
    }

    string webserver_ports_str = "";

    try
    {
        ArgumentParser.parse(args, (ArgumentSyntax syntax)
                             {
                                 syntax.config.caseSensitive = commando.CaseSensitive.yes;
                                 syntax.option('p', "http_ports", &webserver_ports_str, Required.no, "Set frontend ports, example: --http_ports=8081,8082");
                             });
    }
    catch (ArgumentParserException ex)
    {
        stderr.writefln(ex.msg);
        return;
    }

    string[] webserver_ports = webserver_ports_str.split(',');

    if (webserver_ports.length > 0)
    {
        writefln("use options http_ports=%s", webserver_ports);
    }

    string[ string ] env;
    int      exit_code;

    string[] modules =
    [
        "veda", "veda-ccus", "veda-server", "veda-ttlreader", "veda-fanout-email", "veda-fanout-sql", "veda-scripts-main",
        "veda-scripts-lp", "veda-ft-indexer", "veda-ltr-scripts"
    ];
    int[][ string ] command_2_pid;

    bool     is_found_modules = false;

    string[] wr_components =
    [
        "acl_preparer", "fanout_email", "fanout_sql", "fulltext_indexer", "ltr_scripts", "scripts-main", "scripts-lp", "subject_manager",
        "ticket_manager", "acl_preparer"
    ];

    bool is_exist_lock = false;

    foreach (ml; wr_components)
    {
        if (ModuleInfoFile.is_lock(ml) == true)
        {
            writefln("Modile_info [%s] already open, or not deleted lock file", ml);
            is_exist_lock = true;
        }
    }

    if (Queue.is_lock("individuals-flow"))
    {
        writefln("Queue [%s] already open, or not deleted lock file", "individuals-flow");
        is_exist_lock = true;
    }

    if (is_exist_lock)
        return;

    is_found_modules = kill_prev_instance(modules, command_2_pid);

    if (is_found_modules == true)
    {
        writeln("запуск системы невозможен, в памяти остались модули от предыдущего запуска : ", command_2_pid,
                ", завершите их и повторите попытку запуска.");
        return;
    }

    Pid    server_pid;

    string path = "./logs";
    try
    {
        mkdir(path);
        writeln("create folder: ", path);
    }
    catch (Exception ex)
    {
    }

    foreach (ml; modules)
    {
        if (ml != "veda")
        {
            auto _logFile = File("logs/" ~ ml ~ "-stderr.log", "w");
            writeln("start " ~ ml);

            string[] sargs;

            if (need_remove_ontology && ml == "veda-ttlreader")
                sargs = [ "./" ~ ml, "remove-ontology" ];
            else if (need_reload_ontology && ml == "veda-ttlreader")
                sargs = [ "./" ~ ml, "reload-ontology" ];
            else
                sargs = [ "./" ~ ml ];

            auto _pid = spawnProcess(sargs,
                                     std.stdio.stdin,
                                     std.stdio.stdout,
                                     _logFile, env, Config.suppressConsole);

            if (ml == "veda-server")
            {
                server_pid = _pid;
            }
        }
    }


    if (webserver_ports.length > 0)
    {
        foreach (port; webserver_ports)
        {
            string[] sargs;

            string   ml = "veda-webserver";
            sargs = [ "./" ~ ml, "--http_port=" ~ port ];

            auto _logFile = File("logs/" ~ ml ~ port ~ "-stderr.log", "w");
            writeln("start " ~ ml);

            auto _pid = spawnProcess(sargs,
                                     std.stdio.stdin,
                                     std.stdio.stdout,
                                     _logFile, env, Config.suppressConsole);
        }
    }
    else
    {
        string[] sargs;

        string   ml = "veda-webserver";
        sargs = [ "./" ~ ml ];

        auto _logFile = File("logs/" ~ ml ~ "-stderr.log", "w");
        writeln("start " ~ ml);

        auto _pid = spawnProcess(sargs,
                                 std.stdio.stdin,
                                 std.stdio.stdout,
                                 _logFile, env, Config.suppressConsole);
    }

    exit_code = wait(server_pid);

    if (exit_code == -SIGKILL)
        writeln("veda-server terminated, code=", exit_code);

    kill_prev_instance(modules, command_2_pid);

    writefln("EXIT!");
}
