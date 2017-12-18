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
end

if box.space.permissions == nil then
    if memtx then 
        space = box.schema.space.create('acl_indexes')
    else 
        space = box.schema.space.create('acl_indexes', {engine='vinyl'})
    end

    print ('space.acl_indexes:', space.id, '\n')

    box.space.permissions:create_index('primary', {parts={1, 'string'}})
    box.schema.user.grant('guest', 'read,write', 'space', 'permissions')
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

print('ready\n')
