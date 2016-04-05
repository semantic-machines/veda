module bootstrap;

import std.process, std.stdio, std.conv, core.sys.posix.signal;

void main(char[][] args)
{
    int exit_code;

    while (true)
    {
        writeln("start veda-server");

        auto logFile = File("veda-server-errors.log", "w");
        auto pid     = spawnProcess([ "./veda-server" ],
                                    std.stdio.stdin,
                                    std.stdio.stdout,
                                    logFile);

        exit_code = wait(pid);

        if (exit_code == -SIGKILL)
            writeln("veda-server terminated, code=", exit_code);
    }
}
