#! /usr/bin/tarantool
box.cfg {
    listen = 3301;
    io_collect_interval = nil;
    readahead = 16320;
    memtx_memory = 4 * 1024 * 1024 * 1024;
    memtx_min_tuple_size = 16;
    memtx_max_tuple_size = 10 * 1024 * 1024;
    vinyl_max_tuple_size = 10 * 1024 * 1024;
    vinyl_memory = 10 * 1024 * 1024 * 1024;
    vinyl_cache = 1024 * 1024 * 1024;
    vinyl_write_threads = 2;
    wal_mode = "write";
    wal_max_size = 256 * 1024 * 1024;
    checkpoint_interval = 60 * 60;
    checkpoint_count = 6;
    snap_io_rate_limit = nil;
    force_recovery = true;
    log_level = 5;
    log = "./tarantool.log",
    wal_dir = './db/wal',
    memtx_dir = './db/memtx',
    vinyl_dir = './db/vinyl',
--    log_nonblock = true;
    too_long_threshold = 0.5;
    background = false;
    pid_file = 'rust.pid';
}

local function bootstrap()
    box.schema.user.grant('guest', 'read,write,execute', 'universe')
    box.schema.user.create('rust', { password = 'rust' })
    box.schema.user.grant('rust', 'read,write,execute', 'universe')

    box.schema.func.create('libtarantool_authorization.authorization', {language = 'C'})
    box.schema.user.grant('guest', 'execute', 'function', 'libtarantool_authorization.authorization')
end

bootstrap()
local json = require('json')

local msgpack = require('msgpack')
net_box = require('net.box')
capi_connection = net_box:new(3301)

local ffi = require('ffi')
ffi.cdef[[
        void init_dictionaries_ffi();
    ]]
rust = ffi.load('./libtarantool_authorization.so')
rust.init_dictionaries_ffi();
local refresh_dict_fn = function() rust.init_dictionaries_ffi(); end;
box.space._space:on_replace(refresh_dict_fn);
box.space._index:on_replace(refresh_dict_fn);

print("call rust !",json.encode(capi_connection:call('libtarantool_authorization.authorization', {'RU','EUR', 0})))
print("call rust !",json.encode(capi_connection:call('libtarantool_authorization.authorization', {'RU1','EUR1', 0})))

--os.exit();