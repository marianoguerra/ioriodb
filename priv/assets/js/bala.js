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
        options = options || {};
        this.url = url;
        this.options = options;
        this.prefix = prefix;
        this.isPolling = false;
        this.doesHeartBeat = false;
        this.heartBeatId = null;

        if (options.heartBeatIntervalMs === 'number') {
            this.heartBeatIntervalMs = options.heartBeatIntervalMs;
        } else {
            this.heartBeatIntervalMs = Transport.DEFAULT_HEART_BEAT_INTERVAL_MS;
        }
    }

    Transport.DEFAULT_HEART_BEAT_INTERVAL_MS = 5000;

    tproto = Transport.prototype;

    tproto.getUrl = function () {
        return this.prefix + '://' + this.url;
    };

    tproto.connect = notImplemented;
    tproto.send = notImplemented;
    tproto.close = notImplemented;

    // internal function you must call on Transport implementation, you can
    // override it to do some extra work before/after calling onData
    tproto._onData = function () {
        this.onData.apply(this, arguments);
    };

    tproto.onData = noop;
    tproto.onError = noop;
    tproto.onOpen = noop;
    tproto.onClose = noop;
    tproto.onSend = noop;

    tproto.onHeartBeat = noop;

    tproto.stopHeartBeat = function () {
        if (this.heartBeatId) {
            global.clearInterval(this.heartBeatId);
        } else {
            console.warn('Trying to stop non started heartbeat', this);
        }
    };

    tproto.setupHeartBeat = function () {
        // Noop if the transport doesn't support heartbeat
        if (!this.doesHeartBeat) {
            return;
        }

        var self = this;
        this.stopHeartBeat();
        this.heartBeatId = global.setInterval(function () {
            self.onHeartBeat();
        }, this.heartBeatIntervalMs);
    };

    function WebSocketTransport(url, options) {
        var prefix = options.secure ? 'wss' : 'ws';
        Transport.call(this, url, options, prefix);
        this.type = 'ws';
        this._conn = null;
        this.doesHeartBeat = true;
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
            self._onData(evt.data, evt);
        };

        self.setupHeartBeat();
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

    function getNumberOr(obj, defaultVal) {
        if (typeof obj === 'number') {
            return obj;
        } else {
            return defaultVal;
        }
    }

    function XhrTransport(url, options) {
        var prefix = options.secure ? 'https' : 'http';
        Transport.call(this, url, options, prefix);
        this.type = 'xhr';
        this.isPolling = true;
        this._polling = false;
        this._pollReq = null;
        this.options = options || {};

        this._nextPollMs = getNumberOr(options.nextPollMs, 100);
        this._timeoutMs = getNumberOr(options.timeoutMs, 60000);
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

            if (req.responseText !== '') {
                self._onData(req.responseText, evt);
            }
        }

        function error(req, evt, type) {
            self._scheduleNextPoll();
            self.onError(evt, req);
        }

        var self = this,
            url = self.getUrl(),
            baseHeaders = {
                'X-Socket-Transport': 'xhrPolling'
            },
            headers = request.shallowMerge(baseHeaders, this.options.headers),
            req = request.get(url, {
                cache: false,
                headers: headers,
                success: success,
                error: error,
                timeout: self._timeoutMs
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
            this.onClose();
        }
    };

    function SseTransport(url, options) {
        var prefix = options.secure ? 'https' : 'http';
        Transport.call(this, url, options, prefix);
        this.type = 'sse';
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
            self._onData(evt.data, evt);
        };
    };

    sseproto.send = xhrproto.send;

    sseproto.close = function () {
        if (this._conn) {
            this._conn.close();
            this._conn = null;
            this.onClose();
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
