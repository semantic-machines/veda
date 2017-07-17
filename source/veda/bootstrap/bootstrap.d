module veda.bootstrap;

import std.string, std.process, std.stdio, std.conv, core.sys.posix.signal, std.file, core.thread;
import commando;

struct ProcessInfo
{
    int    pid;
    string command;
    string args;
    string stat;
}

extern (C) void handleTermination(int _signal)
{
    writefln("!SYS: VEDA, caught signal: %s", text(_signal));
}

shared static this()
{
    bsd_signal(SIGINT, &handleTermination);
}

//int[ string ] args_2_pid;

private int[ string ] get_processes_info(string[] command_patterns, ref ProcessInfo[ int ] processes)
{
    //writeln("command_patterns=", command_patterns);

    long[ string ] keys;
    int[ string ] args_2_pid;

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
        long STAT    = keys[ "STAT" ];

        foreach (process_str; processes_list[ 1..$ ])
        {
            string[] _data = process_str.split();

            if (_data.length >= keys.length)
            {
                try
                {
                    long   args_start_pos = process_str.indexOf(_data[ COMMAND ]);
                    string _args          = process_str[ args_start_pos..$ ].dup;

                    foreach (pt; command_patterns)
                    {
                        if (_data[ COMMAND ] == pt || _data[ COMMAND ] == "./" ~ pt || _args == pt)
                        {
                            ProcessInfo pi = ProcessInfo(to!int (_data[ PID ]), _data[ COMMAND ], _args, _data[ STAT ]);
                            processes[ pi.pid ] = pi;

                            args_2_pid[ _args ] = pi.pid;
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

    return args_2_pid;
}

import veda.util.module_info;
import veda.util.queue;

bool kill_prev_instance(ref string[] modules)
{
    bool is_found_modules;

    for (int attempt = 0; attempt < 10; attempt++)
    {
        is_found_modules = false;
        ProcessInfo[ int ] processes;

        auto args_2_pid = get_processes_info(modules, processes);

        foreach (ml; modules)
        {
            int pid = args_2_pid.get(ml, -1);
            if (pid != -1)
            {
                if (pid != thisProcessID())
                {
                    is_found_modules = true;
                    kill(pid, SIGKILL);
                    writeln("found running module ", ml, " (", pid, "), kill him.");
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
    int[][ string ] command_2_pids;

    bool need_remove_ontology = false;
    bool need_reload_ontology = false;
    bool need_watchdog = true;

    foreach (arg; args)
    {
        if (arg == "remove-ontology")
            need_remove_ontology = true;
        if (arg == "reload-ontology")
            need_reload_ontology = true;
        if (arg == "no-watchdog")
            need_watchdog = false;
    }

    string webserver_ports_str = "";

    try
    {
        ArgumentParser.parse(args, (ArgumentSyntax syntax)
                             {
                                 syntax.config.caseSensitive = commando.CaseSensitive.yes;
                                 syntax.option('p', "http_ports", &webserver_ports_str, Required.no,
                                               "Set frontend http ports, example: --http_ports=8081,8082");
                             });
    }
    catch (ArgumentParserException ex)
    {
        stderr.writefln(ex.msg);
        return;
    }

    string ext_user_port_str = "";

    try
    {
        ArgumentParser.parse(args, (ArgumentSyntax syntax)
                             {
                                 syntax.config.caseSensitive = commando.CaseSensitive.yes;
                                 syntax.option('e', "ext_usr_http_port", &ext_user_port_str, Required.no,
                                               "Set external user http port, example: --ext_usr_http_port=8082");
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
        "veda", "veda-ccus", "veda-mstorage", "veda-ttlreader", "veda-fanout-email", "veda-fanout-sql-np", "veda-fanout-sql-lp", "veda-scripts-main",
        "veda-scripts-lp", "veda-ft-indexer", "veda-ltr-scripts"
    ];

    bool     is_found_modules = false;

    string[] wr_components =
    [
        "acl_preparer", "fanout_email", "fanout_sql_np", "fanout_sql_lp", "fulltext_indexer", "ltr_scripts", "scripts-main", "scripts-lp",
        "subject_manager",
        "ticket_manager", "acl_preparer"
    ];

    bool is_main_loop = true;

    while (is_main_loop)
    {
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

        is_found_modules = kill_prev_instance(modules);

        if (is_found_modules == true)
        {
            writeln("запуск системы невозможен, в памяти остались модули от предыдущего запуска : ", command_2_pids,
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

        string[] started_modules;

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

                started_modules ~= array_to_str(sargs);

                if (ml == "veda-mstorage")
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

                if (ext_user_port_str.length > 0)
                    sargs ~= "--ext_usr_http_port=" ~ ext_user_port_str;

                auto _logFile = File("logs/" ~ ml ~ port ~ "-stderr.log", "w");
                writeln("start " ~ ml);

                auto _pid = spawnProcess(sargs,
                                         std.stdio.stdin,
                                         std.stdio.stdout,
                                         _logFile, env, Config.suppressConsole);

                started_modules ~= array_to_str(sargs);
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

            started_modules ~= array_to_str(sargs);
        }

        bool is_next = true;


        // wait for all running
        //writeln ("started_modules=", started_modules);
        writeln("wait for all running");
        while (is_next)
        {
            core.thread.Thread.yield();
            core.thread.Thread.sleep(dur!("seconds")(1));

            ProcessInfo[ int ] processes;

            auto args_2_pid = get_processes_info(started_modules, processes);

            is_next = false;
            foreach (ml; started_modules)
            {
                //writeln ("check module ", ml);
                int pid;
                pid = args_2_pid.get(ml, -1);

                if (pid == -1)
                {
                    writeln("not found running module ", ml, " (", pid, ")");
                    is_next = true;
                    break;
                }
            }
        }

        if (need_remove_ontology == true || need_reload_ontology == true || need_watchdog == false)
        {
            exit_code = wait(server_pid);
        }
        else
        {
            writeln("watch dog start");
            is_next = true;
            while (is_next)
            {
                core.thread.Thread.yield();
                core.thread.Thread.sleep(dur!("seconds")(10));

                ProcessInfo[ int ] processes;
                auto args_2_pid = get_processes_info(started_modules, processes);

                foreach (ml; started_modules)
                {
                    //writeln("check module ", ml);
                    int pid;
                    pid = args_2_pid.get(ml, -1);

                    if (pid == -1)
                    {
                        writeln("not found running module ", ml, " (", pid, ")");
                        //string lock_path = "data/module-info/" ~ pi.command ~ ".lock";
                        //writeln ("lock_path=", lock_path);
                        //remove (lock_path);

                        //auto _logFile = File("logs/" ~ ml ~ "-stderr.log", "w");
                        //writeln("restart " ~ ml);
                        //auto _pid = spawnProcess(ml.split (" "),
                        //                         std.stdio.stdin,
                        //                         std.stdio.stdout,
                        //                         _logFile, env, Config.suppressConsole);

                        is_next = false;
                        break;
                    }
                    else
                    {
                        ProcessInfo pi = processes[ pid ];
                        if (pi.stat == "Z+" || pi.stat == "Z")
                        {
                            writeln("defunct module ", pi, " (", pid, ")");
                            kill(pi.pid, SIGKILL);

                            //              string lock_path = "data/module-info/" ~ pi.command ~ ".lock";
                            //              writeln ("lock_path=", lock_path);
                            //              remove (lock_path);


                            //auto _logFile = File("logs/" ~ ml ~ "-stderr.log", "w");
                            //writeln("restart " ~ ml);
                            //auto _pid = spawnProcess(ml.split (" "),
                            //                         std.stdio.stdin,
                            //                         std.stdio.stdout,
                            //                        _logFile, env, Config.suppressConsole);

                            is_next = false;
                            break;
                        }
                    }
                }
            }
        }

        if (exit_code == -SIGKILL)
            writeln("veda-mstorage terminated, code=", exit_code);

        ProcessInfo[ int ] processes;
        auto args_2_pid = get_processes_info(started_modules, processes);

        kill_prev_instance(started_modules);

        executeShell("rm data/module-info/*.lock");
        executeShell("rm data/queue/*.lock");
        executeShell("rm data/uris/*.lock");
    }

    writefln("EXIT!");
}

private string array_to_str(string[] data)
{
    string res;

    foreach (idx, dd; data)
    {
        if (idx > 0)
            res ~= " ";
        res ~= dd;
    }
    return res;
}
