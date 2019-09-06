#! /usr/bin/tarantool
tt_port=3309
box.cfg {
    listen = tt_port;
    io_collect_interval = nil;
    readahead = 16320;
    --memtx_memory = 4 * 1024 * 1024 * 1024;
    --memtx_min_tuple_size = 16;
    --memtx_max_tuple_size = 10 * 1024 * 1024;
    vinyl_max_tuple_size = 1 * 1024 * 1024;
    vinyl_memory = 1 * 1024 * 1024 * 1024;
    vinyl_cache = 102 * 1024 * 1024;
    vinyl_write_threads = 2;
    wal_mode = "write";
    wal_max_size = 256 * 1024 * 1024;
    checkpoint_interval = 60 * 60;
    checkpoint_count = 6;
    snap_io_rate_limit = nil;
    force_recovery = true;
    log_level = 5;
    log = "./tarantool.log",
    --wal_dir = './db/wal',
    --memtx_dir = './db/memtx',
    --vinyl_dir = './db/vinyl',
    work_dir='./db/tarantool',
--    log_nonblock = true;
    too_long_threshold = 0.5;
    background = false;
    pid_file = 'rust.pid';
}

local function bootstrap()

    if box.space.INDIVIDUALS == nil then
	space = box.schema.space.create('INDIVIDUALS')
	print ('create space.individuals:', space.id, '\n')
    end

    if box.space.TICKETS == nil then
	space = box.schema.space.create('TICKETS')
	print ('create space.tickets:', space.id, '\n')
    end

    if box.space.ACL_INDEX == nil then

	space = box.schema.space.create('ACL_INDEX')
	print ('space.acl:', space.id, '\n')

	box.space.ACL:create_index('primary', {parts={1, 'string'}})
	box.schema.user.grant('guest', 'read,write', 'space', 'ACL_INDEX')
--	box.schema.user.grant('guest', 'read,write', 'universe')

	box.schema.user.create('veda6', {password = '123456'}, {if_not_exists = false})
	box.schema.user.grant('veda6', 'read,write,execute', 'universe')
--end
    box.schema.user.grant('guest', 'read,write,execute', 'universe')

    box.schema.user.create('rust', { password = 'rust' })
    box.schema.user.grant('rust', 'read,write,execute', 'universe')

    box.schema.func.create('libtarantool_authorization.authorization', {language = 'C'})
    box.schema.user.grant('guest', 'execute', 'function', 'libtarantool_authorization.authorization')
end

    if box.space.TICKETS_CACHE == nil then
	space = box.schema.space.create('TICKETS_CACHE')
	print ('create space.tickets_cache:', space.id, '\n')
    end

end

bootstrap()
local json = require('json')
local msgpack = require('msgpack')
net_box = require('net.box')
capi_connection = net_box:new(tt_port)

local ffi = require('ffi')
ffi.cdef[[
        void init_dictionaries_ffi();
    ]]
rust = ffi.load('./libtarantool_authorization.so')
rust.init_dictionaries_ffi();
local refresh_dict_fn = function() rust.init_dictionaries_ffi(); end;
box.space._space:on_replace(refresh_dict_fn);
box.space._index:on_replace(refresh_dict_fn);

print("call rust !",json.encode(capi_connection:call('libtarantool_authorization.authorization', {'v-s:appUrl','cfg:VedaSystem', 15})))

--os.exit();