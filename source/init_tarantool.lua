box.cfg{listen=3309, work_dir='./data/tarantool', log_level=5, log='./tarantool.log', memtx_memory=268435456.0}
log = require('log')

memtx = false

if box.space.individuals == nil then
    if memtx then
        space = box.schema.space.create('individuals')
    else
        space = box.schema.space.create('individuals', {engine='vinyl'})
    end 

    print ('space.individuals:', space.id, '\n')

    box.space.individuals:create_index('primary', {parts={1, 'string'}})
    box.schema.user.grant('guest', 'read,write', 'space', 'individuals')
    box.schema.user.grant('guest', 'read,write', 'universe')

    box.schema.user.create('veda6', {password = '8b8nfeAj'}, {if_not_exists = false})
	box.schema.user.grant('veda6', 'read,write,execute', 'universe')

end

if box.space.tickets == nil then
    if memtx then
        space = box.schema.space.create('tickets')
    else
        space = box.schema.space.create('tickets', {engine='vinyl'})
    end

    print ('space.tickets:', space.id, '\n')

    box.space.tickets:create_index('primary', {parts={1, 'string'}})
    box.schema.user.grant('guest', 'read,write', 'space', 'tickets')
end

if box.space.acl_indexes == nil then

    space = box.schema.space.create('acl_indexes')

    print ('space.acl_indexes:', space.id, '\n')

    box.space.acl_indexes:create_index('primary', {parts={1, 'string'}})
    box.schema.user.grant('guest', 'read,write', 'space', 'acl_indexes')
end

print('ready\n')
