/*globals Iorio, console, window, __MakeMXW, __MakeBala, __MakeIorioDB, document*/
function initIorioApp() {
    'use strict';
    var session,
        request = __MakeMXW(window),
        Bala = __MakeBala(window, console, request),
        Iorio = __MakeIorioDB(window, console, request, Bala),

        inputAdminUsername = document.getElementById('admin-username'),
        inputAdminPassword = document.getElementById('admin-password'),

        inputTestUsername = document.getElementById('test-username'),
        inputTestPassword = document.getElementById('test-password'),

        inputBucket = document.getElementById('bucket'),
        inputStream = document.getElementById('stream'),

        txtInput = document.getElementById('event-raw'),
        txtOutput = document.getElementById('output'),

        btnStartTest = document.getElementById('start-test'),
        btnSendEvent = document.getElementById('send-event');

    function getEventData() {
        var data = {
            user: {
                username: inputTestUsername.value,
                password: inputTestPassword.value
            },
            event: {
                bucket: inputBucket.value,
                stream: inputStream.value,
                data: txtInput.value
            }
        };

        try {
            data.event.data = JSON.parse(data.event.data);
        } catch (error) {
            window.alert("invalid json");
            throw error;
        }

        return data;
    }

    function getTestData() {
        var obj = getEventData();
        obj.admin =  {
            username: inputAdminUsername.value,
            password: inputAdminPassword.value
        };

        return obj;
    }

    function toString(value) {
        try {
            return JSON.stringify(value, null, 2);
        } catch (_) {
            return '' + value;
        }
    }

    function onErrorLog(txt) {
        return function (event) {
            log('Error', txt, event);
        };
    }

    function onStartTextClick() {
        var data = getTestData();

        session = new Iorio.Session('localhost:8080', data.admin.username, data.admin.password);

        log('start test', data);
        session.authenticate(onAuthOk, onAuthError);
    }

    function onSendEventClick() {
        var data = getEventData(),
            e = data.event;

        function success(response) {
            log('Event Response', response);
        }

        log('send event', data);
        session.sendToStream(e.bucket, e.stream, e.data,
                             success, onErrorLog('send event'));
    }

    btnStartTest.addEventListener('click', onStartTextClick);
    btnSendEvent.addEventListener('click', onSendEventClick);

    function log() {
        txtOutput.innerHTML += Array.prototype.slice.call(arguments).map(toString).join(' ') + '\n';
    }

    function onAuthOk(token) {
        log('authenticated', token);
        connect(token);
    }

    function onAuthError(err) {
        log('Error Authenticating', err);
    }

    function connect(token) {
        var subs = session.makeSubscriptions(),
            subsSse, subsWs, subsXhr, subsBest;

        subsSse = session.makeSubscriptions({connection: {transport: 'sse'}});
        subsWs = session.makeSubscriptions({connection: {transport: 'ws'}});
        subsXhr = session.makeSubscriptions({connection: {transport: 'xhr'}});
        subsBest = session.makeSubscriptions();

        function logEvent(eventType) {
            return function (event) {
                log("Event", eventType, event);
            };
        }

        subsXhr.onOpen = logEvent('open xhr');
        subsXhr.onClose = logEvent('close xhr');
        subsXhr.onData = logEvent('data xhr');
        subsXhr.onError = logEvent('error xhr');

        subsBest.onOpen = logEvent('open best');
        subsBest.onClose = logEvent('close best');
        subsBest.onData = logEvent('data best');
        subsBest.onError = logEvent('error best');

        subsSse.onOpen = logEvent('open sse');
        subsSse.onClose = logEvent('close sse');
        subsSse.onData = logEvent('data sse');
        subsSse.onError = logEvent('error sse');

        subsWs.onOpen = logEvent('open ws');
        subsWs.onClose = logEvent('close ws');
        subsWs.onData = logEvent('data ws');
        subsWs.onError = logEvent('error ws');

        window._iorioXhr = subsXhr;
        window._iorioSse = subsSse;
        window._iorioWs = subsWs;
        window._iorioBest = subsBest;
        window._iorioSess = session;

        window._iorioSubscribe = function (bucket, stream, fromId) {
            subsXhr.subscribe(bucket, stream, fromId);
            subsSse.subscribe(bucket, stream, fromId);
            subsWs.subscribe(bucket, stream, fromId);
            subsBest.subscribe(bucket, stream, fromId);
        };

        window._iorioUnsubscribe = function (bucket, stream) {
            subsXhr.unsubscribe(bucket, stream);
            subsSse.unsubscribe(bucket, stream);
            subsWs.unsubscribe(bucket, stream);
            subsBest.unsubscribe(bucket, stream);
        };

        window._iorioSubscribe('mariano', 'test');

        subsXhr.connect();
        subsSse.connect();
        subsWs.connect();
        subsBest.connect();
    }
}
