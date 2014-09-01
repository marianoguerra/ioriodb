/*globals $, document, window*/
function disruptApp(document, window, $) {
    'use strict';
    var input = document.getElementById('input'),
        output = document.getElementById('output'),
        nickInput = document.getElementById('nick'),
        send = document.getElementById('send'),

        connection;

    function sendMessage(text) {
        var nick = getNick();
        connection.send(nick + ': ' + text);
    }

    function onSendClicked() {
        var text = input.value.trim();

        if (text !== '') {
            sendMessage(text);
        }

        input.value = '';
    }

    function getNick() {
        var nick = nickInput.value.trim();

        if (nick === '') {
            return 'anonymous';
        } else {
            return nick;
        }
    }

    function notify(text) {
        var date = (new Date()).toLocaleString();
        output.innerHTML = output.innerHTML + '[' + date + '] ' + text + '\n';
    }

    function onData(data) {
        notify(data);
    }

    send.addEventListener('click', onSendClicked);

    function start(url, options, notify, onData) {
        var connection = $.bullet(url, options);

        connection.onopen = function(){
            notify('online'); 
        };

        connection.onclose = connection.ondisconnect = function(){
            notify('offline');
        };

        connection.onmessage = function(e){
            onData(e.data);
        };

        return connection;
    }

    connection = start('ws://localhost:8080/listen', {}, notify, onData);
}

document.addEventListener("DOMContentLoaded", function() {
    'use strict';
    disruptApp(document, window, $);
});
