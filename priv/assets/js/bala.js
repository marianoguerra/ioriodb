function __MakeBala(global, console, request) {
    'use strict';
    var transportPriority = ['ws', 'sse', 'xhr'],
        transportSupport = {
            ws: !!request.WebSocket,
            xhr: true,
            sse: !!request.EventSource
        },
        transports,
        tproto, wsproto, xhrproto, sseproto;

    function noop() {
    }

    function notImplemented() {
        throw new Error('Not Implemented');
    }

    function Transport (url, options, prefix) {
        this.url = url;
        this.options = options;
        this.prefix = prefix;
    }

    tproto = Transport.prototype;

    tproto.getUrl = function () {
        return this.prefix + '://' + this.url;
    };

    tproto.connect = notImplemented;
    tproto.send = notImplemented;
    tproto.close = notImplemented;

    tproto.onData = noop;
    tproto.onOpen = noop;
    tproto.onClose = noop;
    tproto.onSend = noop;

    function WebSocketTransport(url, options) {
        var prefix = options.secure ? 'wss' : 'ws';
        Transport.call(this, url, options, prefix);
        this._conn = null;
    }

    wsproto = new Transport();
    WebSocketTransport.prototype = wsproto;
    wsproto.constructor = WebSocketTransport;

    wsproto.connect = function () {
        var self = this;

        if (self._conn) {
            console.warn('calling connect on an already connected transport', this);
            return;
        }

        self._conn = new request.WebSocket(self.getUrl());

        self._conn.onclose = function (evt) {
            self.onClose(evt);
        };

        self._conn.onopen = function (evt) {
            self.onOpen(evt);
        };

        self._conn.onerror = function (evt) {
            self.onError(evt);
        };

        self._conn.onmessage = function (evt) {
            self.onData(evt.data, evt);
        };
    };

    wsproto.send = function (data) {
        this._conn.send(data);
        this.onSend(data);
    };

    wsproto.close = function () {
        if (this._conn) {
            this._conn.close();
            this._conn = null;
        }
    };

    function XhrTransport(url, options) {
        var prefix = options.secure ? 'https' : 'http';
        Transport.call(this, url, options, prefix);
        this._polling = false;
        this._pollReq = null;

        if (typeof options.nextPollMs === 'number') {
            this._nextPollMs = options.nextPollMs;
        } else {
            this._nextPollMs = 100;
        }
    }

    xhrproto = new Transport();
    XhrTransport.prototype = xhrproto;
    xhrproto.constructor = XhrTransport;

    xhrproto._scheduleNextPoll = function () {
        var self = this;

        global.setTimeout(function () {
            self._poll();
        }, self._nextPollMs);
    };

    xhrproto._poll = function () {
        this._polling = true;
        function success(req, evt, type) {
            self._scheduleNextPoll();

            if (req.status !== 204) {
                self.onData(req.responseText, evt);
            }
        }

        function error(req, evt, type) {
            self._scheduleNextPoll();
            self.onError(evt, req);
        }

        var self = this,
            url = self.getUrl(),
            headers = {
                'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8',
                'X-Socket-Transport': 'xhrPolling'
            },
            req = request.get(url, {
                cache: false,
                headers: headers,
                success: success,
                error: error
            });

        self.onOpen({_type: 'xhr'}, req);

        this._pollReq = req;
    };

    xhrproto.connect = function () {
        if (!this._polling) {
            this._poll();
        } else {
            console.warn('calling connect on an already connected transport', this);
        }
    };

    xhrproto.send = function (data) {
        function success(req, evt, type) {
            self.onSend(data);

            if (req.status !== 204) {
                self.onData(req.responseText);
            }
        }

        function error(req, evt, type) {
            self.onError(evt, req);
        }

        var self = this,
            url = self.getUrl(),
            headers = {
                'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8',
                'X-Socket-Transport': 'xhrPolling'
            },
            req = request.post(url, {
                body: data,
                headers: headers,
                success: success,
                error: error
            });
    };

    xhrproto.close = function () {
        if (this._pollReq) {
            this._pollReq.abort();
            this._pollReq = null;
        }
    };

    function SseTransport(url, options) {
        var prefix = options.secure ? 'https' : 'http';
        Transport.call(this, url, options, prefix);
        this._conn = null;
    }

    sseproto = new Transport();
    SseTransport.prototype = sseproto;
    sseproto.constructor = SseTransport;

    sseproto.connect = function () {
        var self = this,
            url = self.getUrl();

        if (self._conn) {
            console.warn('calling connect on an already connected transport', this);
            return;
        }

        self._conn = new request.EventSource(url);

        self._conn.onopen = function (evt) {
            self.onOpen(evt);
        };

        self._conn.onerror = function (evt) {
            self.onError(evt);
        };

        self._conn.onmessage = function (evt) {
            self.onData(evt.data, evt);
        };

        self.onOpen({_type: 'sse'}, self._conn);
    };

    sseproto.send = xhrproto.send;

    sseproto.close = function () {
        if (this._conn) {
            this._conn.close();
            this._conn = null;
        }
    };

    transports = {
        ws: WebSocketTransport,
        xhr: XhrTransport,
        sse: SseTransport
    };

    function getBestTransportId(options) {
        var transportsToTry, i, len, transportId;

        if (!options.transport) {
            transportsToTry = transportPriority;

        } else if (!Array.isArray(options.transport)) {
            transportsToTry = [options.transport];
        } else {
            transportsToTry = options.transport;
        }

        for (i = 0, len = transportsToTry.length; i < len; i += 1) {
            transportId = transportsToTry[i];

            if (transportSupport[transportId]) {
                return transportId;
            }
        }

        return null;
    }

    function connection(url, options) {
        options = options || {};
        var transportId = getBestTransportId(options),
            Transport = transports[transportId];
            
        if (!Transport) {
            throw new Error('No suitable transport found');
        }

        return new Transport(url, options);
    }

    return {
        connection: connection,
        _transport: {
            getBest: getBestTransportId,
            support: transportSupport,
            priority: transportPriority,
            transports: transports
        }
    };
}
