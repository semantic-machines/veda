// Server backend

const ServerBackend = {};

export default ServerBackend;

ServerBackend.status = 'limited';

ServerBackend.get_rights = function (ticket, uri, user_id) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    uri = arg.uri;
    user_id = arg.user_id;
  }
  try {
    const json = get_rights(ticket, uri, user_id);
    if (json) {
      return Promise.resolve(json);
    } else {
      return Promise.reject(Error('get_rights failed'));
    }
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.query = function (ticket, queryStr, sort, databases, top, limit, from) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    queryStr = arg.query;
    sort = arg.sort;
    databases = arg.databases;
    top = arg.top;
    limit = arg.limit;
    from = arg.from;
  }
  try {
    return Promise.resolve( query(ticket, queryStr, sort, databases, top, limit, from) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.get_individual = function (ticket, uri) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    uri = arg.uri;
  }
  try {
    const json = get_individual(ticket, uri);
    if (json) {
      return Promise.resolve(json);
    } else {
      return Promise.reject(Error('Not found'));
    }
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.get_individuals = function (ticket, uris) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    uris = arg.uris;
  }
  try {
    return Promise.resolve( get_individuals(ticket, uris) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.remove_individual = function (ticket, uri) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    uri = arg.uri;
  }
  try {
    return Promise.resolve( remove_individual(ticket, uri) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.put_individual = function (ticket, individual) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    individual = arg.individual;
  }
  try {
    return Promise.resolve( put_individual(ticket, individual) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.add_to_individual = function (ticket, individual) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    individual = arg.individual;
  }
  try {
    return Promise.resolve( add_to_individual(ticket, individual) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.set_in_individual = function (ticket, individual) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    individual = arg.individual;
  }
  try {
    return Promise.resolve( set_in_individual(ticket, individual) );
  } catch (err) {
    return Promise.reject(err);
  }
};

ServerBackend.remove_from_individual = function (ticket, individual) {
  const arg = ticket;
  const isObj = typeof arg === 'object';
  if (isObj) {
    ticket = arg.ticket;
    individual = arg.individual;
  }
  try {
    return Promise.resolve( remove_from_individual(ticket, individual) );
  } catch (err) {
    return Promise.reject(err);
  }
};
