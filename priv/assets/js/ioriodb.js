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
                self.token = body.token;
                success(body.token, body, req);
            } else {
                error(req, evt);
            }
        }

        request.json.post(url, {body: body, success: onLoad, error: error});
    };

    lib.Session = Session;

    return lib;
}
