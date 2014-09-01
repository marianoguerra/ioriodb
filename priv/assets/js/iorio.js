/*globals $, document, window, console*/
(function (document, global, $, console) {
    'use strict';
    var iorio;

    function Iorio(url, options) {
        var self = this;
        this._id = 0;
        this.url = url;
        this.options = options;
        this.connection = $.bullet(url, options);
        this.subscriptions = [];

        self.onOpen = $.Callbacks();
        self.onClose = $.Callbacks();
        self.onDisconnect = $.Callbacks();
        self.onData = $.Callbacks();
        self.onSend = $.Callbacks();
        self.onPing = $.Callbacks();
        self.onSubscribe = $.Callbacks();
        self.onUnsubscribe = $.Callbacks();

        this.connection.onopen = function () {
            self.onOpen.fire();
        };

        this.connection.onclose = function () {
            self.onClose.fire();
        };

        this.connection.ondisconnect = function () {
            self.onDisconnect.fire();
        };

        this.connection.onmessage = function (e) {
            self.onData.fire(JSON.parse(e.data), e);
        };

        this.connection.onheartbeat = function () {
            self.ping();
        };
    }

    Iorio.prototype.sendMessage = function (msg) {
        this.onSend.fire(msg);
        return this.connection.send(JSON.stringify(msg));
    };

    Iorio.prototype.subscribe = function (bucket, stream) {
        this.subscriptions.push({
            bucket: bucket,
            stream: stream
        });

        this.onSubscribe.fire(bucket, stream);
        return this.sendMessage({
            cmd: "subscribe",
            bucket: bucket,
            stream: stream,
            id: this.nextId()
        });
    };

    Iorio.prototype.unsubscribe = function (bucket, stream) {
        this.subscriptions = this.subscriptions.filter(function (item) {
            return item.bucket  !== bucket || item.stream !== stream;
        });
        this.onUnsubscribe.fire(bucket, stream);
        return this.sendMessage({
            cmd: "unsubscribe",
            bucket: bucket,
            stream: stream,
            id: this.nextId()
        });
    };

    Iorio.prototype.nextId = function () {
        this._id += 1;
        return this._id;
    };

    Iorio.prototype.ping = function () {
        this.onPing.fire();
        return this.sendMessage({
            cmd: "ping",
            id: this.nextId()
        });
    };

    global.Iorio = Iorio;
}(document, window, $, console));

