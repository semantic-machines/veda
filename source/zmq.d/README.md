zmq.d
====

A higher level, idiomatic zmq wrapper for the D language.

Features
=========

* wrappers for common zmq methods
* resource management and destruction
* policy based concurrency support
* vibe.d support ( integration with vibe.d event loop )


```D

/// default concurrency - as is, blocking
import dzmq;
auto ctx = new ZmqContext();
auto sock = ctx.socket!(ZmqSocketType.Req)();
sock.connect("tcp://localhost:1234")
```

```D
/// vibe.d concurrency - fibers ,yields execution while waiting
import dzmq;
import dzmq.concurrency_mixins;

auto ctx = new ZmqContext();
auto sock = ctx.socket!(ZmqSocketType.Req, VibeDConcurrencyPolicy)();
sock.connect("tcp://localhost:1234");
```

TBD
====

* Callbacks on messages
* Threading support

