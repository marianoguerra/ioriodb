function __MakeIorioDB(global, console, request) {
    'use strict';
    var lib = {},
        sproto;

    function Session(baseUrl, username, password, options) {
        this.baseUrl = baseUrl;
        this.username = username;
        this.password = password;
        this.token = null;
    }

    sproto = Session.prototype;
    sproto.authenticate = function (success, error) {
        var self = this,
            url = request.join(self.baseUrl, 'sessions'),
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
        var url = request.join(this.baseUrl, 'buckets');
        this._getJson(url, success, error);
    };

    sproto.listStreams = function (bucket, success, error) {
        var url = request.join(this.baseUrl, 'streams', bucket);
        this._getJson(url, success, error);
    };

    sproto.getSession = function (success, error) {
        var url = request.join(this.baseUrl, 'sessions');
        this._getJson(url, success, error);
    };

    sproto.ping = function (success, error) {
        var url = request.join(this.baseUrl, 'ping');
        this._getJson(url, success, error, false);
    };

    // for now username and password on the user object
    sproto.createUser = function (user, success, error) {
        var url = request.join(this.baseUrl, 'users');
        this._postJson(url, user, success, error);
    };

    sproto.sendToStream = function (bucket, stream, data, success, error) {
        var url = request.join(this.baseUrl, 'streams', bucket, stream);
        this._postJson(url, data, success, error);
    };

    // s.patchStream('m', 's', [{op: "add", path: "/age", value: 29}], success, error)
    sproto.patchStream = function (bucket, stream, patch, success, error) {
        var url = request.join(this.baseUrl, 'streams', bucket, stream);
        this._patchJson(url, patch, success, error);
    };

    sproto.queryStream = function (bucket, stream, fromId, limit, success, error) {
        var url = request.join(this.baseUrl, 'streams', bucket, stream),
            prefix = '?';

        if (fromId !== null && fromId !== undefined) {
            url += prefix + 'from=' + fromId;
            prefix = '&';
        }

        if (limit !== null && limit !== undefined) {
            url += prefix + 'limit=' + limit;
            prefix = '&';
        }

        this._getJson(url, success, error);
    };

    lib.Session = Session;

    return lib;
}
