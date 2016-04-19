module veda.bootstrap;

import std.process, std.stdio, std.conv, core.sys.posix.signal;

void main(char[][] args)
{
    int  exit_code;

    auto fanout_logFile = File("veda-fanout-errors.log", "w");

    writeln("start veda-fanout");
    auto pid_fanout = spawnProcess([ "./veda-fanout" ],
                                   std.stdio.stdin,
                                   std.stdio.stdout,
                                   fanout_logFile);

    while (true)
    {
        auto server_logFile = File("veda-server-errors.log", "w");
        writeln("start veda-server");
        auto pid = spawnProcess([ "./veda-server" ],
                                std.stdio.stdin,
                                std.stdio.stdout,
                                server_logFile);
        exit_code = wait(pid);

        if (exit_code == -SIGKILL)
            writeln("veda-server terminated, code=", exit_code);
    }
}
