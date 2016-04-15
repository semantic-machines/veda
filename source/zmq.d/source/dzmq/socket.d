module dzmq.socket;

import dzmq.concurrency_mixins;

import deimos.zmq.zmq;
import std.conv: to;
import std.string;
import std.algorithm: find, remove;
import std.stdio;
import std.exception;

enum ZmqSocketType{
    Pair = ZMQ_PAIR,
    Pub = ZMQ_PUB,
    Sub = ZMQ_SUB,
    Req = ZMQ_REQ,
    Rep = ZMQ_REP,
    Dealer = ZMQ_DEALER,
    Router = ZMQ_ROUTER,
    Pull = ZMQ_PULL,
    Push = ZMQ_PUSH,
    XPub = ZMQ_XPUB,
    XSub = ZMQ_XSUB
}

class ZmqTimeout: Exception {
    this(string msg = "Operation timedout"){
        super(msg);
    }
}

class ZmqException: ErrnoException {
    this(string msg) {
        super(msg);
    }
}

class ZmqContext {
    private {
        immutable void * _ctx;
        bool _terminated = false;
        this (immutable void * ctx) {
            _ctx = ctx;
        }
    }

    this(int threads=1){
        _ctx = cast(immutable(void*))zmq_init(threads);
    }
    
    ~this() {
        terminate();
    }

    auto dup() {
        return new ZmqContext(_ctx);
    }

    void terminate(){
        if (_terminated) return;
        zmq_term(cast(void*)_ctx);
        debug { writeln("Terminated ctx"); }
        _terminated = true;
    }

    auto ref socket(ZmqSocketType sockType, alias ConcurrencyPolicy = DefaultConcurrencyPolicy)(){
        return new ZmqSocket!(sockType, ConcurrencyPolicy)(zmq_socket(cast(void*)_ctx, cast(int)sockType));
    }
    
    ZmqSocketReq socket_req(){
        return new ZmqSocketReq (new ZmqSocket!(ZmqSocketType.Req, DefaultConcurrencyPolicy)(zmq_socket(cast(void*)_ctx, cast(int)ZmqSocketType.Req)));
    } 
}

private mixin template WritableProperties() {
    @property bool can_write() {
        int events;
        size_t events_size = events.sizeof;
        zmq_getsockopt(_sock, ZMQ_EVENTS, &events, &events_size);
        return (events & ZMQ_POLLOUT) == ZMQ_POLLOUT;
    }
}

private mixin template ReadableProperties() {
    @property bool can_read() {
        int events;
        size_t events_size = events.sizeof;
        zmq_getsockopt(_sock, ZMQ_EVENTS, &events, &events_size);
        return (events & ZMQ_POLLIN) == ZMQ_POLLIN;
    }
}

class ZmqSocketReq
{
	this (ZmqSocket!(ZmqSocketType.Req, DefaultConcurrencyPolicy) _ftc)
	{
		ftc = _ftc;
	}
	
	ZmqSocket!(ZmqSocketType.Req, DefaultConcurrencyPolicy) ftc; 	
}

class ZmqSocket(ZmqSocketType type, alias ConcurrencyPolicy = DefaultConcurrencyPolicy) {
    mixin ConcurrencyPolicy;

    private {
        void * _sock;
        ZmqSocketType _type = type;
        bool _closed = false;
        bool _busy = true;
        
        this(void * socket) {
            _sock = socket;
        }
    }

    ~this() {
        close();
    }
    
	void set_options (byte opt_id, int opt_value)
	{
		int rc = zmq_setsockopt(_sock, opt_id, &opt_value, opt_value.sizeof);		
        if (rc!=0) throw new ZmqException("Error on set opt");
	}
	
    void close() {
        if (_closed) return;
        static if(is(typeof(__onConnect))) __onClose();
        debug { writeln("Closed socket"); }
        int rc = zmq_close(_sock);
        _closed = true;
        if (rc!=0) throw new ZmqException("Error on close");
    }

    void connect(string endpoint) {
        int rc = zmq_connect(_sock, endpoint.toStringz);
        writeln("Connecting rc is %d", rc);
        if (rc!=0) throw new ZmqException("Error on connecting");

        static if(is(typeof(__onConnect))) __onConnect();
    }

    void disconnect(string endpoint = null) {
        int rc = zmq_disconnect(_sock, endpoint.toStringz);
        if (rc!=0) throw new ZmqException("Error on disconnect");
    }

    void bind(string endpoint) {
        int rc = zmq_bind(_sock, endpoint.toStringz);
        if (rc!=0) throw new ZmqException("Error on binding");

        static if(is(typeof(__onConnect))) __onBind();
    }

    void unbind(string endpoint) {
        int rc = zmq_unbind(_sock, endpoint.toStringz);
        if (rc!=0) throw new ZmqException("Error on unbind");
    }

    @property bool isBusy() { return _busy; }

    @property void identity(string ident) {
        zmq_setsockopt(_sock, ZMQ_IDENTITY, ident.ptr, ident.length);
    }

    static if (type==ZmqSocketType.Pub || type==ZmqSocketType.XPub || type==ZmqSocketType.Push) {
        mixin WritableProperties;
        mixin WriteableSocket;
    } else static if (type==ZmqSocketType.Sub || type==ZmqSocketType.XSub || type==ZmqSocketType.Pull) {
        mixin ReadableProperties;
        mixin ReadableSocket;
    } else {
        mixin WritableProperties;
        mixin WriteableSocket;

        mixin ReadableProperties;
        mixin ReadableSocket;
    }

    static if (type==ZmqSocketType.Sub) {
        void subscribe(string sub) {
            zmq_setsockopt(_sock, ZMQ_SUBSCRIBE, sub.ptr, sub.length);
        }
    }
}
        
version(unittest) {
    import std.concurrency;
    import std.stdio;
    import std.stdio;
    import core.thread;

    void start_reading(shared ZmqContext ctx) {
        receive(
                (bool start) { writeln("starting");}
               );

        auto inctx = cast(ZmqContext)ctx;
        auto subsock = inctx.socket!(ZmqSocketType.Sub)();
        subsock.subscribe("");
        writeln("connecting");
        subsock.connect("tcp://localhost:54321");
        send(ownerTid, true);
        auto res = subsock.receive();
        writeln(res);

        send(ownerTid, true);
    }

    unittest {
        auto ctx = new ZmqContext();
        auto task = spawn(&start_reading, cast(shared)ctx);

        auto sock = ctx.socket!(ZmqSocketType.Pub)();
        sock.bind("tcp://*:54321");
        send(task, true);
        auto read = receiveOnly!(bool);
        writeln("writing");
        Thread.getThis().sleep(1.msecs);
        sock.send(["help", "me"]);

        auto success = receiveOnly!(bool);
        assert(success);
    }
}

