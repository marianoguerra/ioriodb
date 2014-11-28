/*globals Iorio, console, window, __MakeMXW, __MakeBala, __MakeIorioDB*/
(function () {
    'use strict';
    var request = __MakeMXW(window),
        Bala = __MakeBala(window, console, request),
        Iorio = __MakeIorioDB(window, console, request),
        session = new Iorio.Session('http://localhost:8080', 'admin', 'secret');

    function onAuthOk(token) {
        console.log('authenticated', token);
        connect(token);
    }

    function onAuthError(err) {
        console.error('Error Authenticating', err);
    }

    session.authenticate(onAuthOk, onAuthError);

    function connect(token) {
        var encodedToken = window.encodeURIComponent(token),
            path = '/listen?s=mariano:testa:10&s=mariano:test&jwt=' + encodedToken,
            url = 'localhost:8080' + path,
            connSse = Bala.connection(url, {transport: 'sse'}),
            connWs = Bala.connection(url, {transport: 'ws'}),
            connXhr = Bala.connection(url, {transport: 'xhr'}),
            connBest = Bala.connection(url);

        console.log('Listening to', url);

        function logEvent(eventType) {
            return function (event) {
                console.log("Event", eventType, event);
            };
        }

        connXhr.onOpen = logEvent('open xhr');
        connXhr.onClose = logEvent('close xhr');
        connXhr.onData = logEvent('data xhr');
        connXhr.onError = logEvent('error xhr');

        connBest.onOpen = logEvent('open best');
        connBest.onClose = logEvent('close best');
        connBest.onData = logEvent('data best');
        connBest.onError = logEvent('error best');

        connSse.onOpen = logEvent('open sse');
        connSse.onClose = logEvent('close sse');
        connSse.onData = logEvent('data sse');
        connSse.onError = logEvent('error sse');

        connWs.onOpen = logEvent('open ws');
        connWs.onClose = logEvent('close ws');
        connWs.onData = logEvent('data ws');
        connWs.onError = logEvent('error ws');

        connXhr.connect();
        connSse.connect();
        connWs.connect();
        connBest.connect();

        window._iorioXhr = connXhr;
        window._iorioSse  = connSse;
        window._iorioWs = connWs;
        window._iorioBest = connBest;
        window._iorioSess = session;
    }
}(this));
