module veda.core.queue;

import std.conv;
import veda.type, veda.core.know_predicates, veda.core.define, veda.core.context;
import veda.core.storage.lmdb_storage;
import veda.onto.onto, veda.onto.individual, veda.onto.resource;
import veda.core.util.cbor8individual;

// ////// logger ///////////////////////////////////////////
import util.logger;
logger _log;
logger log()
{
    if (_log is null)
        _log = new logger("veda-core-" ~ process_name, "log", "queue");
    return _log;
}
// ////// ////// ///////////////////////////////////////////

enum QMessageType
{
    STRING = 'S',
    OBJECT = 'O'
}

struct QueueInfo
{
        long       first_element;
        long       last_element;
}

static string q_prefix        = "q:";
static string q_name          = "q:name";
static string q_first_element = "q:first_element";
static string q_last_element  = "q:last_element";

class Queue
{
    private LmdbStorage storage;
    QueueInfo[string] queues;

    this(string context_name)
    {
        storage = new LmdbStorage(queue_db_path, DBMode.R, context_name ~ ":queue");
    }

    public void push(string _queue_name, string msg, QMessageType type)
    {
        string     queue_name = q_prefix ~ _queue_name;
        // 1. get queue info
        long       first_element;
        long       last_element;
        Individual queue = storage.find_individual(queue_name);

        if (queue !is Individual.init)
        {
            first_element = queue.getFirstInteger(q_first_element, 0);
            last_element  = queue.getFirstInteger(q_last_element, 0);
        }
        else
        {
            first_element = 1;
            last_element  = 0;

            queue.uri = queue_name;
            queue.addResource(q_first_element, Resource(first_element));
            queue.addResource(q_name, Resource(queue_name));
        }

        last_element++;
        queue.setResources(q_last_element, [ Resource(last_element) ]);

        string key = queue_name ~ "_" ~ text(last_element);

        string s_queue = individual2cbor(&queue);

        storage.put(queue_name, s_queue);
        storage.put(key, type ~ msg);
    }

    public string pop(string _queue_name)
    {
        string     queue_name = q_prefix ~ _queue_name;

        long       first_element;
        long       last_element;
        Individual queue = storage.find_individual(queue_name);

        if (queue !is Individual.init)
        {
            first_element = queue.getFirstInteger(q_first_element, 0);
            last_element  = queue.getFirstInteger(q_last_element, 0);
        }

        string key   = queue_name ~ "_" ~ text(first_element);
        string value = storage.find(key);

        storage.remove(key);

        return value;
    }
    
    private void load_queues_info ()
    {
 //   	storage.get_of_cursor(bool delegate (string key, string value) prepare)
    }
    
}
