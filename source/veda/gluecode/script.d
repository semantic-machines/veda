module veda.gluecode.script;

import std.stdio, std.path;
import veda.gluecode.v8d_header, veda.core.common.context;

struct ScriptInfo
{
    string id;
    string str_script;
    bool[ string ] filters;
    Script compiled_script;
}
