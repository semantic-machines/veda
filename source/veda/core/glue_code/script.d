module veda.core.glue_code.script;

import veda.core.bind.v8d_header, veda.core.common.context;

struct ScriptInfo
{
    string id;
    string str_script;
    bool[ string ] filters;
    Script compiled_script;
}

