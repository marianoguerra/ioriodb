function __MakeIorioDB(global, console, request, Bala) {
    'use strict';
    var lib = {},
        sproto, subprt, subsprt;

    function Session(baseUrl, username, password, options) {
        this.baseUrl = baseUrl;
        this.username = username;
        this.password = password;
        this.token = null;
        this.rawToken = null;

        if (options && options.secure) {
            this.url = 'https://' + baseUrl;
        } else {
            this.url = 'http://' + baseUrl;
        }

    }

    sproto = Session.prototype;
    sproto.authenticate = function (success, error) {
        var self = this,
            url = request.join(self.url, 'sessions'),
            body = {username: self.username, password: self.password};

        function onLoad(req, evt) {
            var body;
            if (req.status === 201) {
                body = JSON.parse(req.responseText);
                self.rawToken = body.token;
                self.token = global.encodeURIComponent(body.token);
                success(body.token, body, req);
            } else {
                error(req, evt);
            }
        }

        request.json.post(url, {body: body, success: onLoad, error: error});
    };

    sproto.makeSubscriptions = function (options) {
        return new Subscriptions(this, options);
    };

    sproto._patchJson = function (url, patch, success, error) {
        var self = this;
        function onLoad(req, evt) {
            var body;
            if (req.status === 200) {
                body = JSON.parse(req.responseText);
                success(body, req);
            } else {
                error(req, evt);
            }
        }

        request.json.patch(url, {
            headers: {
                'x-session': self.rawToken,
                'Content-Type': 'application/json-patch+json'
            },
            success: onLoad,
            error: error,
            body: patch
        });
    };

    sproto._postJson = function (url, data, success, error) {
        var self = this;
        function onLoad(req, evt) {
            var body;
            if (req.status === 201) {
                body = JSON.parse(req.responseText);
                success(body, req);
            } else {
                error(req, evt);
            }
        }

        request.json.post(url, {
            headers: {'x-session': self.rawToken},
            success: onLoad,
            error: error,
            body: data
        });
    };

    sproto._getJson = function (url, success, error, cache) {
        var self = this;
        function onLoad(req, evt) {
            var body;
            if (req.status === 200) {
                body = JSON.parse(req.responseText);
                success(body, req);
            } else {
                error(req, evt);
            }
        }

        request.json.get(url, {
            headers: {'x-session': self.rawToken},
            success: onLoad,
            error: error,
            cache: cache
        });
    };

    sproto.listBuckets = function (success, error) {
        var url = request.join(this.url, 'buckets');
        this._getJson(url, success, error);
    };

    sproto.listStreams = function (bucket, success, error) {
        var url = request.join(this.url, 'streams', bucket);
        this._getJson(url, success, error);
    };

    sproto.getSession = function (success, error) {
        var url = request.join(this.url, 'sessions');
        this._getJson(url, success, error);
    };

    sproto.ping = function (success, error) {
        var url = request.join(this.url, 'ping');
        this._getJson(url, success, error, false);
    };

    // for now username and password on the user object
    sproto.createUser = function (user, success, error) {
        var url = request.join(this.url, 'users');
        this._postJson(url, user, success, error);
    };

    sproto.sendToStream = function (bucket, stream, data, success, error) {
        var url = request.join(this.url, 'streams', bucket, stream);
        this._postJson(url, data, success, error);
    };

    // s.patchStream('m', 's', [{op: "add", path: "/age", value: 29}], success, error)
    sproto.patchStream = function (bucket, stream, patch, success, error) {
        var url = request.join(this.url, 'streams', bucket, stream);
        this._patchJson(url, patch, success, error);
    };

    sproto.queryStream = function (bucket, stream, fromId, limit, success, error) {
        var url = request.join(this.url, 'streams', bucket, stream),
            prefix = '?';

        if (fromId !== null && fromId !== undefined) {
            url += prefix + 'from=' + fromId;
            prefix = '&';
        }

        if (limit !== null && limit !== undefined) {
            url += prefix + 'limit=' + limit;
            prefix = '&';
        }

        this._getJson(url, success, error, false);
    };

    // don't mutate it
    function Subscription(bucket, stream, fromId) {
        this.bucket = bucket;
        this.stream = stream;
        this.fromId = fromId;
        this.key = Subscription.key(bucket, stream);
    }

    Subscription.key = function (bucket, stream) {
        return bucket + ':' + stream;
    };

    subprt = Subscription.prototype;
    subprt.toString = function () {
        if (typeof this.fromId === 'number') {
            return this.key + ':' + this.fromId;
        } else {
            return this.key;
        }
    };

    function Subscriptions(session, options) {
        options = options || {};
        this.session = session;
        this._subs = {};
        this._conn = null;
        this._options = options;
        this._connOptions = options.connection || {};
        this._connProps = null;
    }

    Subscriptions._baseConnectionProps = {
        sse: {
        },
        ws: {
        },
        xhr: {
        }
    };

    subsprt = Subscriptions.prototype;

    subsprt._reconnect = function () {
        if (this._conn) {
            this._conn.close();
            this._conn = null;
        }

        this.connect();
    };

    subsprt._connSubscribe = function (sub) {
        if (this._conn === null) {
            return;
        }

        switch (this._conn.type) {
            case 'xhr':
            case 'sse':
                this._reconnect();
                break;
            case 'ws':
                this.send({
                    cmd: 'subscribe',
                    bucket: sub.bucket,
                    stream: sub.stream,
                });
                break;
            default:
                console.warn('Unknown transport type', this._conn);
        }
    };

    subsprt._connUnsubscribe = function (sub) {
        if (this._conn === null) {
            return;
        }

        switch (this._conn.type) {
            case 'xhr':
            case 'sse':
                this._reconnect();
                break;
            case 'ws':
                this.send({
                    cmd: 'unsubscribe',
                    bucket: sub.bucket,
                    stream: sub.stream,
                });
                break;
            default:
                console.warn('Unknown transport type', this._conn);
        }
    };

    subsprt.subscribe = function (bucket, stream, fromId) {
        var sub = new Subscription(bucket, stream, fromId);
        this._subs[sub.key] = sub;
        this._connSubscribe(sub);
    };

    subsprt.unsubscribe = function (bucket, stream) {
        var sub = new Subscription(bucket, stream);
        delete this._subs[sub.key];
        this._connUnsubscribe(sub);
    };

    subsprt.map = function (fun) {
        var accum = [],
            key, val;

        for (key in this._subs) {
            val = this._subs[key];
            accum.push(fun(val));
        }

        return accum;
    };

    subsprt.toString = function () {
        return this.map(function (sub) {
            return 's=' + sub.toString();
        }).join('&');
    };

    subsprt.getUrl = function () {
        var self = this,
            subsStr = self.toString(),
            token = self.session.token,
            url = request.join(self.session.baseUrl, 'listen'),
            conn;

        if (subsStr !== '') {
            url = request.appendQueryParam(url, subsStr);
        }

        url = request.appendQueryParam(url, 'jwt=' + token);

        return url;
    };

    subsprt.send = function (data) {
        if (this._conn) {
            this._conn.send(JSON.stringify(data));
        } else {
            console.warn('Trying to send data with no connection', this);
        }
    };

    subsprt._getConnection = function () {
        // shallow clone it
        var options = request.shallowMerge(this._connOptions);

        if (this._conn === null) {
            var conn,
                self = this,
                url = self.getUrl();

            conn = Bala.connection(url, options);
            conn._onData = function (dataRaw, evt) {
                var data = JSON.parse(dataRaw);
                self.update(data);
                conn.onData(data, evt);
            };

            conn.onHeartBeat = function () {
                self.send({cmd: 'ping'});
            };

            conn.onData = function () {
                self.onData.apply(self, arguments);
            };

            conn.onError = function () {
                self.onError.apply(self, arguments);
            };

            conn.onOpen = function () {
                self.onOpen.apply(self, arguments);
            };

            conn.onClose = function () {
                self.onClose.apply(self, arguments);
            };

            conn.onSend = function () {
                self.onSend.apply(self, arguments);
            };

            self._conn = conn;
            self._connProps = Subscriptions._baseConnectionProps[conn.type];
        }

        return this._conn;
    };

    subsprt.connect = function () {
        this._getConnection().connect();
    };

    subsprt.onData = function (data, evt) {
        console.log.apply(console, arguments);
    };
    subsprt.onError = function (evt, ctx) {
        console.error.apply(console, arguments);
    };
    // onOpen and onClose may be called multiple times if the connection does
    // polling or requires to be restarted to reflect subscription changes
    subsprt.onOpen = function (evt, ctx) {};
    subsprt.onClose = function (evt, ctx) {};
    subsprt.onSend = function (data) {};

    subsprt.update = function (events) {
        var self = this,
            conn;

        if (!Array.isArray(events)) {
            return;
        }

        events.forEach(function (event) {
            var meta = event.meta,
                bucket = meta.bucket,
                stream = meta.stream,
                key = Subscription.key(bucket, stream),
                id = meta.id,
                sub = self._subs[key];

            if (sub) {
                sub.fromId = id;
            }
        });

        self._updateConnection();
    };

    subsprt._updateConnection = function () {
        if (this._conn && this._conn.isPolling) {
            this._conn.url = this.getUrl();
        }
    };

    lib.Session = Session;

    return lib;
}
