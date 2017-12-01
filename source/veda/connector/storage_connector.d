module veda.connector.storage_connector;

private
{
    import core.thread;
    import std.stdio;
    import backtrace.backtrace, Backtrace = backtrace.backtrace;
    import msgpack;
    import veda.common.type, veda.connector.requestresponse, veda.common.logger;
    import std.socket;
}

const MAX_SIZE_OF_PACKET = 1024 * 1024 * 10;

class TTStorageConnector
{
    Logger           log;
    public ubyte[]   buf;
    public string    addr;
    public ushort    port;

    public TcpSocket s;

    this(Logger _log)
    {
        log = _log;
    }

    public void connect(string addr, ushort port)
    {
        this.addr = addr;
        this.port = port;

        s = new TcpSocket();
        for (;; )
        {
            try
            {
                log.trace("CONNECT STD %s %d", addr, port);
                s.connect(new InternetAddress(addr, port));
            }
            catch (Exception e)
            {
                Thread.sleep(dur!("seconds")(3));
                continue;
            }
            break;
        }
        log.trace("CONNECTED STD");
    }


    public RequestResponse put(OptAuthorize op_auth, string user_uri, string[] binobj_individuals, bool trace = true)
    {
        bool need_auth;

        if (op_auth == OptAuthorize.YES)
            need_auth = true;

        ubyte[]         response;
        RequestResponse request_response = new RequestResponse();

        if (user_uri is null || user_uri.length < 3)
        {
            request_response.common_rc = ResultCode.Not_Authorized;
            log.trace("ERR! StorageConnector:put, code=%s", request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }
        if (binobj_individuals.length == 0)
        {
            request_response.common_rc = ResultCode.No_Content;
            log.trace("ERR! StorageConnector:put, code=%s", request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }

        Packer packer = Packer(false);


        if (trace)
            log.trace("@ StorageConnector:put PACK PUT REQUEST");

        packer.beginArray(binobj_individuals.length + 3);
        packer.pack(INDV_OP.PUT, need_auth, user_uri);
        for (int i = 0; i < binobj_individuals.length; i++)
            packer.pack(binobj_individuals[ i ]);

        long request_size = packer.stream.data.length;
        if (trace)
            log.trace("@ StorageConnector:put DATA SIZE %d", request_size);

        buf = new ubyte[ 4 + request_size ];

        buf[ 0 ]               = cast(byte)((request_size >> 24) & 0xFF);
        buf[ 1 ]               = cast(byte)((request_size >> 16) & 0xFF);
        buf[ 2 ]               = cast(byte)((request_size >> 8) & 0xFF);
        buf[ 3 ]               = cast(byte)(request_size & 0xFF);
        buf[ 4 .. buf.length ] = packer.stream.data;

        for (;; )
        {
            if (trace)
                log.trace("@ StorageConnector:put send buf");

            s.send(buf);
            buf.length = 4;
            long receive_size = s.receive(buf);
            if (trace)
                log.trace("@ StorageConnector:put RECEIVE SIZE BUF %d", receive_size);


            if (trace)
                log.trace("@ StorageConnector:put RESPONSE SIZE BUF %s", buf);
            long response_size = 0;
            for (int i = 0; i < 4; i++)
                response_size = (response_size << 8) + buf[ i ];
            if (trace)
                log.trace("@ StorageConnector:put RESPONSE SIZE %d", response_size);

            if (response_size > MAX_SIZE_OF_PACKET)
            {
                request_response.common_rc = ResultCode.Size_too_large;
                log.trace("ERR! StorageConnector:put, code=%s", request_response.common_rc);
                return request_response;
            }

            response = new ubyte[ response_size ];

            receive_size = s.receive(response);
            if (trace)
                log.trace("@ StorageConnector:put RECEIVE RESPONSE %d", receive_size);

            if (receive_size == 0 || receive_size < response.length)
            {
                Thread.sleep(dur!("seconds")(1));
                log.trace("WARN! StorageConnector:put, receive_size(%d) < response.length(%d), RECONNECT PUT REQUEST", receive_size, response.length);
                close();
                connect(addr, port);
                continue;
            }
            break;
        }

        StreamingUnpacker unpacker =
            StreamingUnpacker(response);

        if (unpacker.execute())
        {
            auto obj = unpacker.unpacked[ 0 ];
            request_response.common_rc      = cast(ResultCode)(obj.via.uinteger);
            request_response.op_rc.length   = unpacker.unpacked.length - 1;
            request_response.binobjs.length = 0;

            if (trace)
                log.trace("@ StorageConnector:put OP RESULT = %d", obj.via.uinteger);
            for (int i = 1; i < unpacker.unpacked.length; i++)
            {
                obj                             = unpacker.unpacked[ i ];
                request_response.op_rc[ i - 1 ] = cast(ResultCode)obj.via.uinteger;
                if (trace)
                    log.trace("@ StorageConnector:put PUT RESULT = %d", obj.via.uinteger);
            }
            if (trace)
                log.trace("@ StorageConnector:put OP_RC %s", request_response.op_rc);
        }
        else
            log.trace("ERR! StorageConnector:put, ON UNPACKING RESPONSE");

        return request_response;
    }

    public RequestResponse get(OptAuthorize op_auth, string user_uri, string[] uris, bool trace)
    {
        bool need_auth;

        if (op_auth == OptAuthorize.YES)
            need_auth = true;

        ubyte[]         response;
        RequestResponse request_response = new RequestResponse();

        if (user_uri is null || user_uri.length < 3)
        {
            request_response.common_rc = ResultCode.Not_Authorized;
            log.trace("ERR! connector.get[%s], code=%s", uris, request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }
        if (uris.length == 0)
        {
            request_response.common_rc = ResultCode.No_Content;
            log.trace("ERR! connector.get[%s], code=%s", uris, request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }

        Packer packer = Packer(false);

        //need_auth = false;

        if (trace)
            log.trace("connector.get PACK GET REQUEST need_auth=%b, user_uri=%s, uris=%s", need_auth, user_uri, uris);

        packer.beginArray(uris.length + 3);
        packer.pack(INDV_OP.GET, need_auth, user_uri);
        for (int i = 0; i < uris.length; i++)
            packer.pack(uris[ i ]);

        long request_size = packer.stream.data.length;

        if (trace)
            log.trace("connector.get DATA SIZE %d", request_size);

        buf = new ubyte[ 4 + request_size ];

        buf[ 0 ]               = cast(byte)((request_size >> 24) & 0xFF);
        buf[ 1 ]               = cast(byte)((request_size >> 16) & 0xFF);
        buf[ 2 ]               = cast(byte)((request_size >> 8) & 0xFF);
        buf[ 3 ]               = cast(byte)(request_size & 0xFF);
        buf[ 4 .. buf.length ] = packer.stream.data;

        for (;; )
        {
            s.send(buf);

            if (trace)
                log.trace("connector.get SEND %s", buf);

            buf.length = 4;
            long receive_size = s.receive(buf);

            if (trace)
                log.trace("connector.get RECEIVE SIZE BUF %d", receive_size);

            if (trace)
                log.trace("connector.get RESPONSE SIZE BUF %s", buf);

            long response_size = 0;
            for (int i = 0; i < 4; i++)
                response_size = (response_size << 8) + buf[ i ];

            if (trace)
                log.trace("connector.get RESPONSE SIZE %d", response_size);

            if (response_size > MAX_SIZE_OF_PACKET)
            {
                log.trace("connector.get RESPONSE SIZE BUF %s %s", buf, cast(char[])buf);

                request_response.common_rc = ResultCode.Size_too_large;
                log.trace("ERR! connector.get[%s], code=%s", uris, request_response.common_rc);
                return request_response;
            }

            response = new ubyte[ response_size ];

            receive_size = s.receive(response);
            if (trace)
                log.trace("connector.get RECEIVE RESPONSE %s", receive_size);

            if (receive_size == 0 || receive_size < response.length)
            {
                Thread.sleep(dur!("seconds")(1));
                log.trace("connector.get @RECONNECT GET REQUEST");
                close();
                connect(addr, port);
                continue;
            }
            break;
        }


        StreamingUnpacker unpacker =
            StreamingUnpacker(response);
        if (unpacker.execute())
        {
            auto obj = unpacker.unpacked[ 0 ];
            request_response.common_rc      = cast(ResultCode)(obj.via.uinteger);
            request_response.op_rc.length   = unpacker.unpacked.length - 1;
            request_response.binobjs.length = uris.length;

            if (trace)
                log.trace("connector.get OP RESULT = %d", obj.via.uinteger);

            for (int i = 1, j = 0; i < unpacker.unpacked.length; i += 2, j++)
            {
                obj                         = unpacker.unpacked[ i ];
                request_response.op_rc[ j ] = cast(ResultCode)obj.via.uinteger;
                if (request_response.op_rc[ j ] == ResultCode.OK)
                    request_response.binobjs[ j ] = cast(string)unpacker.unpacked[ i + 1 ].via.raw;
                if (trace)
                    log.trace("connector.get GET RESULT = %d", obj.via.uinteger);
            }
        }
        else
            log.trace("connector.get @ERR ON UNPACKING RESPONSE");

        return request_response;
    }

    public RequestResponse authorize(string user_uri, string[] uris, bool trace,
                                     bool trace_auth = false)
    {
        ubyte[]         response;
        RequestResponse request_response = new RequestResponse();

        if (user_uri is null || user_uri.length < 3)
        {
            request_response.common_rc = ResultCode.Not_Authorized;
            log.trace("ERR! connector.authorize, code=%s", request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }
        if (uris.length == 0)
        {
            request_response.common_rc = ResultCode.No_Content;
            log.trace("ERR! connector.authorize, code=%s", request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }

        Packer packer = Packer(false);

        //need_auth = false;

        if (trace)
            log.trace("connector.authorize PACK AUTHORIZE REQUEST user_uri=%s, uris=%s", user_uri, uris);

        packer.beginArray(uris.length + 4);
        packer.pack(INDV_OP.AUTHORIZE, false, trace_auth, user_uri);
        /+ packer.beginArray(uris.length + 3);
           packer.pack(INDV_OP.AUTHORIZE, false, user_uri);+/
        for (int i = 0; i < uris.length; i++)
            packer.pack(uris[ i ]);

        long request_size = packer.stream.data.length;

        if (trace)
            log.trace("connector.authorize DATA SIZE %d", request_size);

        buf = new ubyte[ 4 + request_size ];

        buf[ 0 ]               = cast(byte)((request_size >> 24) & 0xFF);
        buf[ 1 ]               = cast(byte)((request_size >> 16) & 0xFF);
        buf[ 2 ]               = cast(byte)((request_size >> 8) & 0xFF);
        buf[ 3 ]               = cast(byte)(request_size & 0xFF);
        buf[ 4 .. buf.length ] = packer.stream.data;

        for (;; )
        {
            s.send(buf);

            if (trace)
                log.trace("connector.authorize SEND %s", buf);


            buf.length = 4;
            long receive_size = s.receive(buf);

            if (trace)
                log.trace("connector.authorize RECEIVE SIZE BUF %d", receive_size);

            if (trace)
                log.trace("connector.authorize RESPONSE SIZE BUF %s %s", buf, cast(char[])buf);

            long response_size = 0;
            for (int i = 0; i < 4; i++)
                response_size = (response_size << 8) + buf[ i ];

            if (trace)
                log.trace("connector.authorize RESPONSE SIZE %d", response_size);

            if (response_size > MAX_SIZE_OF_PACKET)
            {
                request_response.common_rc = ResultCode.Size_too_large;
                log.trace("ERR! connector.authorize, code=%s", request_response.common_rc);
                return request_response;
            }

            response = new ubyte[ response_size ];

            receive_size = s.receive(response);
            if (trace)
                log.trace("connector.authorize RECEIVE RESPONSE %s", receive_size);

            if (receive_size == 0 || receive_size < response.length)
            {
                Thread.sleep(dur!("seconds")(1));
                log.trace("connector.authorize @RECONNECT AUTHORIZE REQUEST");
                close();
                connect(addr, port);
                continue;
            }
            break;
        }


        StreamingUnpacker unpacker = StreamingUnpacker(response);

        if (unpacker.execute())
        {
            auto obj = unpacker.unpacked[ 0 ];
            request_response.common_rc     = cast(ResultCode)(obj.via.uinteger);
            request_response.op_rc.length  = unpacker.unpacked.length - 1;
            request_response.rights.length = uris.length;

            if (trace)
                log.trace("connector.authorize OP RESULT = %d, unpacker.unpacked.length=%d", obj.via.uinteger, unpacker.unpacked.length);

            for (int i = 1, j = 0; i < unpacker.unpacked.length; i += 3, j++)
            {
                obj                          = unpacker.unpacked[ i ];
                request_response.op_rc[ j ]  = cast(ResultCode)obj.via.uinteger;
                request_response.rights[ j ] = cast(ubyte)unpacker.unpacked[ i + 1 ].via.uinteger;
                if (trace)
                    log.trace("connector.authorize AUTHORIZE RESULT: op_rc=%d, right=%d", request_response.op_rc[ j ], request_response.rights[ j ]);
            }
        }
        else
            log.trace("connector.authorize @ERR ON UNPACKING RESPONSE");

        return request_response;
    }

    public RequestResponse remove(OptAuthorize op_auth, string user_uri, string[] uris, bool trace)
    {
        bool need_auth;

        if (op_auth == OptAuthorize.YES)
            need_auth = true;

        ubyte[]         response;
        RequestResponse request_response = new RequestResponse();

        if (user_uri is null || user_uri.length < 3)
        {
            request_response.common_rc = ResultCode.Not_Authorized;
            log.trace("ERR! connector.remove: need_auth=%s, user_uri=%s, uris=[%s] code=%s", need_auth, user_uri, uris, request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }
        if (uris.length == 0)
        {
            request_response.common_rc = ResultCode.No_Content;
            log.trace("ERR! connector.remove: need_auth=%s, user_uri=%s, uris=[%s] code=%s", need_auth, user_uri, uris, request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }

        Packer packer = Packer(false);

        //need_auth = false;

        if (trace)
            log.trace("connector.get PACK REMOVE REQUEST need_auth=%b, user_uri=%s, uris=%s", need_auth, user_uri, uris);

        packer.beginArray(uris.length + 3);
        packer.pack(INDV_OP.REMOVE, need_auth, user_uri);
        for (int i = 0; i < uris.length; i++)
            packer.pack(uris[ i ]);

        long request_size = packer.stream.data.length;

        if (trace)
            log.trace("connector.remove DATA SIZE %d", request_size);

        buf = new ubyte[ 4 + request_size ];

        buf[ 0 ]               = cast(byte)((request_size >> 24) & 0xFF);
        buf[ 1 ]               = cast(byte)((request_size >> 16) & 0xFF);
        buf[ 2 ]               = cast(byte)((request_size >> 8) & 0xFF);
        buf[ 3 ]               = cast(byte)(request_size & 0xFF);
        buf[ 4 .. buf.length ] = packer.stream.data;

        for (;; )
        {
            s.send(buf);

            if (trace)
                log.trace("connector.remove SEND %s", buf);


            buf.length = 4;
            long receive_size = s.receive(buf);

            if (trace)
                log.trace("connector.remove RECEIVE SIZE BUF %d", receive_size);

            if (trace)
                log.trace("connector.remove RESPONSE SIZE BUF %s", buf);

            long response_size = 0;
            for (int i = 0; i < 4; i++)
                response_size = (response_size << 8) + buf[ i ];

            if (trace)
                log.trace("connector.remove RESPONSE SIZE %d", response_size);

            if (response_size > MAX_SIZE_OF_PACKET)
            {
                request_response.common_rc = ResultCode.Size_too_large;
                log.trace("ERR! connector.remove, code=%s", request_response.common_rc);
                return request_response;
            }

            response = new ubyte[ response_size ];

            receive_size = s.receive(response);
            if (trace)
                log.trace("connector.remove RECEIVE RESPONSE %s", receive_size);

            if (receive_size == 0 || receive_size < response.length)
            {
                Thread.sleep(dur!("seconds")(1));
                log.trace("connector.remove @RECONNECT REMOVE REQUEST");
                close();
                connect(addr, port);
                continue;
            }
            break;
        }


        StreamingUnpacker unpacker =
            StreamingUnpacker(response);

        if (unpacker.execute())
        {
            auto obj = unpacker.unpacked[ 0 ];
            request_response.common_rc      = cast(ResultCode)(obj.via.uinteger);
            request_response.op_rc.length   = unpacker.unpacked.length - 1;
            request_response.binobjs.length = uris.length;

            if (trace)
                log.trace("connector.remove OP RESULT = %d", obj.via.uinteger);
            stderr.writefln("connector.remove OP RESULT = %d", obj.via.uinteger);

            for (int i = 1; i < unpacker.unpacked.length; i++)
            {
                obj                             = unpacker.unpacked[ i ];
                request_response.op_rc[ i - 1 ] = cast(ResultCode)obj.via.uinteger;
            }
        }
        else
            log.trace("connector.remove @ERR ON UNPACKING RESPONSE");

        return request_response;
    }

    void close()
    {
        s.close();
    }

    public RequestResponse get_ticket(string[] ticket_ids, bool trace)
    {
        ubyte[]         response;
        RequestResponse request_response = new RequestResponse();

        // stderr.writefln("@TICKET IDS %s", ticket_ids);

        if (ticket_ids.length == 0)
        {
            request_response.common_rc = ResultCode.No_Content;
            log.trace("ERR! connector.get_ticket[%s], code=%s", ticket_ids, request_response.common_rc);
            printPrettyTrace(stderr);
            return request_response;
        }

        Packer packer = Packer(false);

        //need_auth = false;

        if (trace)
            log.trace("connector.get_ticket PACK GET_TICKET REQUEST ticket_ids=[%s]", ticket_ids);

        packer.beginArray(ticket_ids.length + 3);
        packer.pack(INDV_OP.GET_TICKET, false, "cfg:VedaSystem");
        for (int i = 0; i < ticket_ids.length; i++)
            packer.pack(ticket_ids[ i ]);

        long request_size = packer.stream.data.length;

        if (trace)
            log.trace("connector.get_ticket DATA SIZE %d", request_size);

        buf = new ubyte[ 4 + request_size ];

        buf[ 0 ]               = cast(byte)((request_size >> 24) & 0xFF);
        buf[ 1 ]               = cast(byte)((request_size >> 16) & 0xFF);
        buf[ 2 ]               = cast(byte)((request_size >> 8) & 0xFF);
        buf[ 3 ]               = cast(byte)(request_size & 0xFF);
        buf[ 4 .. buf.length ] = packer.stream.data;

        for (;; )
        {
            s.send(buf);

            if (trace)
                log.trace("connector.get_ticket SEND %s", buf);

            buf.length = 4;
            long receive_size = s.receive(buf);

            if (trace)
                log.trace("connector.get_ticket RECEIVE SIZE BUF %d", receive_size);

            if (trace)
                log.trace("connector.get_ticket RESPONSE SIZE BUF %s", buf);

            long response_size = 0;
            for (int i = 0; i < 4; i++)
                response_size = (response_size << 8) + buf[ i ];

            if (trace)
                log.trace("connector.get_ticket RESPONSE SIZE %d", response_size);

            if (response_size > MAX_SIZE_OF_PACKET)
            {
                log.trace("connector.get RESPONSE SIZE BUF %s %s", buf, cast(char[])buf);

                request_response.common_rc = ResultCode.Size_too_large;
                log.trace("ERR! connector.get_ticket[%s], code=%s", ticket_ids, request_response.common_rc);
                return request_response;
            }

            response = new ubyte[ response_size ];

            receive_size = s.receive(response);

            if (trace)
                log.trace("connector.get_ticket RECEIVE RESPONSE %s", receive_size);

            if (receive_size == 0 || receive_size < response.length)
            {
                Thread.sleep(dur!("seconds")(1));
                log.trace("connector.get_ticket @RECONNECT GET_TICKET REQUEST");
                close();
                connect(addr, port);
                continue;
            }
            break;
        }


        StreamingUnpacker unpacker =
            StreamingUnpacker(response);
        if (unpacker.execute())
        {
            auto obj = unpacker.unpacked[ 0 ];
            request_response.common_rc      = cast(ResultCode)(obj.via.uinteger);
            request_response.op_rc.length   = unpacker.unpacked.length - 1;
            request_response.binobjs.length = ticket_ids.length;

            if (trace)
                log.trace("connector.get_ticket OP RESULT = %d", obj.via.uinteger);

            for (int i = 1, j = 0; i < unpacker.unpacked.length; i += 2, j++)
            {
                obj                         = unpacker.unpacked[ i ];
                request_response.op_rc[ j ] = cast(ResultCode)obj.via.uinteger;
                // stderr.writeln("@J ", j, request_response.op_rc[ j ]);
                if (request_response.op_rc[ j ] == ResultCode.OK)
                    request_response.binobjs[ j ] = cast(string)unpacker.unpacked[ i + 1 ].via.raw;
                if (trace)
                    log.trace("connector.get_ticket GET RESULT = %d", obj.via.uinteger);
            }
        }
        else
            log.trace("connector.get_ticket @ERR ON UNPACKING RESPONSE");

        return request_response;
    }
}

