module dzmq.concurrency_mixins;


mixin template DefaultConcurrencyPolicy() {

    mixin template SocketClose() {}
    mixin template SocketBind() {}
    mixin template SocketConnect() {}

    mixin template WriteableSocket() {

        void send(string[] frames) {
            scope (exit) { _busy = false; }
            _busy = true;

            zmq_msg_t req;
            int rc;

            if (frames.length==1) {
                rc = zmq_send(_sock, frames[0].ptr, frames[0].length, 0);
                if (rc<0) throw new ZmqException("Error on write");
                return;
            }

            foreach(frame; frames[0..frames.length-1]){
                rc = zmq_send(_sock, frame.ptr, frame.length, ZMQ_SNDMORE);
                if (rc<0) throw new ZmqException("Error on write");
            }
            rc = zmq_send(_sock, frames[frames.length-1].ptr, frames[frames.length-1].length, 0);
            if (rc<0) throw new ZmqException("Error on write");
        }
    }

    mixin template ReadableSocket() {
        string[] receive() {
            scope (exit) { _busy = false; }
            _busy = true;

            char* buffer;
            int rc;
            zmq_msg_t frame;
            int more;
            size_t more_size = more.sizeof;

            scope string[] frames;
            do {
                zmq_msg_init(&frame);
                rc = zmq_recvmsg(_sock, &frame, 0);
                if (rc<0) throw new ZmqException("Error on read");
                buffer = (cast(char*)zmq_msg_data(&frame));
                frames ~= buffer[0..zmq_msg_size(&frame)].to!string;
                zmq_getsockopt(_sock, ZMQ_RCVMORE, &more, &more_size);
                zmq_msg_close(&frame);
            } while(more);

            return frames.dup;
        }
    }
}

version(Have_vibe_d) {
    pragma(msg, "Compiling dzmq with vibe.d support");

    public import vibe.d;
    public import vibe.core.core;
    public import core.time;

    mixin template VibeDConcurrencyPolicy() {

        private {
            FileDescriptorEvent _fd_read_evt;
            FileDescriptorEvent _fd_write_evt;
            int _fd;
            TaskCondition _readCondition;
            TaskMutex _readMutex;
            TaskMutex _writeMutex;
            Task _reader;
        }

        void __onClose() {
            _fd_read_evt.destroy();
            _fd_write_evt.destroy();
        }

        void __onConnect() {
            size_t fd_size = _fd.sizeof;
            zmq_getsockopt(_sock, ZMQ_FD, &_fd, &fd_size);
            _fd_read_evt = createFileDescriptorEvent(_fd, FileDescriptorEvent.Trigger.read, false);
            _fd_write_evt = createFileDescriptorEvent(_fd, FileDescriptorEvent.Trigger.write, false);

            _readMutex = new TaskMutex;
            _writeMutex = new TaskMutex;
            _readCondition = new TaskCondition(_readMutex);
        }

        void __onBind() {
            size_t fd_size = _fd.sizeof;
            zmq_getsockopt(_sock, ZMQ_FD, &_fd, &fd_size);
            _fd_read_evt = createFileDescriptorEvent(_fd, FileDescriptorEvent.Trigger.read, false);
            _fd_write_evt = createFileDescriptorEvent(_fd, FileDescriptorEvent.Trigger.write, false);

            _readMutex = new TaskMutex;
            _writeMutex = new TaskMutex;
            _readCondition = new TaskCondition(_readMutex);
        }

        mixin template WriteableSocket() {
            void send(string[] frames, Duration timeout=100.msecs) {
		        scope(exit) _busy=false;

                synchronized(_writeMutex) {
                    _busy = true;
                    if (!_fd_write_evt.wait(timeout, FileDescriptorEvent.Trigger.write)){
                        throw new ZmqTimeout();
                    }
                    zmq_msg_t req;
                    int rc;


                    if (frames.length==1) {
                        rc = zmq_send(_sock, frames[0].ptr, frames[0].length, 0);
                        if (rc<0) throw new ZmqException("Error on write");
                        return;
                    } else {
                        foreach(part; frames[0..frames.length-1]){
                            rc = zmq_send(_sock, part.ptr, part.length, ZMQ_SNDMORE);
                            if (rc<0) throw new ZmqException("Error on write");
                        }
                        rc = zmq_send(_sock, frames[frames.length-1].ptr, frames[frames.length-1].length, 0);
                        if (rc<0) throw new ZmqException("Error on write");
                    }

                    // we need that to reset the zmq_events as per the zmq manual
                    int events;
                    size_t events_size = events.sizeof;
                    zmq_getsockopt(_sock, ZMQ_EVENTS, &events, &events_size);
                }
            }
        }

        mixin template ReadableSocket() {
            string[] receive(Duration timeout=100.msecs) {
                scope(exit) _busy=false;

                synchronized(_readMutex) {
                    _busy = true;
                    char* buffer;
                    int rc;
                    zmq_msg_t frame;
                    int more;
                    size_t more_size = more.sizeof;

                    do {
                        _fd_read_evt.wait(FileDescriptorEvent.Trigger.read);
                        if (!can_read ) { continue;	}

                        scope string[] frames;
                        do {
                            zmq_msg_init(&frame);
                            rc = zmq_recvmsg(_sock, &frame, 0);
                            if (rc<0) throw new ZmqException("Error on read");
                            buffer = (cast(char*)zmq_msg_data(&frame));
                            frames ~= buffer[0..zmq_msg_size(&frame)].to!string;
                            zmq_getsockopt(_sock, ZMQ_RCVMORE, &more, &more_size);
                            zmq_msg_close(&frame);
                        } while(more && can_read);

                        return frames.dup;
                    } while(!can_read);

                    assert(0);
                }
                
            }
        }
    }
}
