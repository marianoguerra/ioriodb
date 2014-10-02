/*globals $, document, window, console, Iorio, React, JsonHuman*/
function startApp(document, window, $, console, Iorio) {
    'use strict';
    var iorio,
        output = document.getElementById('traffic'),
        mConnections, mConnection, mNewConnection, mConnectionManager, mLogin,
        mPage,
        conns, token;

    function addTraffic(data) {
        var node = JsonHuman.format(data);
        output.appendChild(node);
    }

    function post(path, data) {
        return $.ajax({
            type: 'POST',
            url: path,
            contentType: 'application/json',
            data: JSON.stringify(data),
            dataType: 'json',
            headers: {
                'x-session': token
            }
        });
    }

    mLogin = React.createClass({
        displayName: 'LoginForm',
        getInitialState: function() {
            return {username: '', password: '', error: ''};
        },
        textFor: function (name) {
            return this.refs[name].getDOMNode().value.trim();
        },
        login: function (username, password) {
            console.log('login', username, password);
            return post('/sessions', {username: username, password: password});
        },
        onLogin: function () {
            var username = this.textFor('username'),
                password = this.textFor('password'),
                self = this;

            if (username === '') {
                this.state.error = 'Username Empty';
                this.setState(this.state);
                return;
            }

            if (password === '') {
                this.state.error = 'Password Empty';
                this.setState(this.state);
                return;
            }

            this.state.error = '';

            this.login(username, password)
                .done(function (response) {
                    self.state.password = '';
                    if (response.ok) {
                        console.log('login succeeded', response);
                        token = response.token;
                        iorio = new Iorio('ws://localhost:8080/listen?jwt=' + token, {});
                        iorio.onSend.add(addTraffic);
                        iorio.onData.add(addTraffic);

                    } else {
                        console.warn('login failed', response);
                        self.state.error = 'login failed';
                    }

                    self.setState(self.state);
                })
                .fail(function (error) {
                    self.state.password = '';
                    self.state.error = 'login error';
                    self.setState(self.state);
                    console.log('error on login', error);
                });
        },
        render: function() {
            var self = this;
            return React.DOM.div({className: "login-form"},
                                 formRow('username', 'Username'),
                                 formRow('password', 'Password', 'password'),
                                 text(this.state.error, 'error'),
                                 button("Login", this.onLogin, "login-action"));
        }
    });

    mConnections = React.createClass({
        displayName: 'Connections',
        getInitialState: function() {
            return {items: [], error: ''};
        },
        add: function (bucket, stream) {
            var item = {bucket: bucket, stream: stream},
                wasThere = this.remove(item);

            this.state.items.push(item);

            if (!wasThere) {
                iorio.subscribe(bucket, stream);
            }

            this.setState(this.state);

            return wasThere;
        },
        remove: function (item) {
            var lenBefore = this.state.items.length,
                lenAfter, changed;
            this.state.items = this.state.items.filter(function (obj) {
                return item.stream !== obj.stream || item.bucket !== obj.bucket;
            });

            lenAfter = this.state.items.length;
            changed = lenBefore > lenAfter;

            if (changed) {
                iorio.unsubscribe(item.bucket, item.stream);
            }

            this.setState(this.state);
            return changed;
        },
        componentDidMount: function() {
            this.setState({items: []});
        },
        textFor: function (name) {
            return this.refs[name].getDOMNode().value.trim();
        },
        onPing: function () {
            iorio.ping();
        },
        onAdd : function () {
            var bucket = this.textFor('bucket'),
                stream = this.textFor('stream');

            if (bucket === '') {
                this.state.error = 'Bucket Empty';
                return;
            }

            if (stream === '') {
                this.state.error = 'Stream Empty';
                return;
            }

            this.state.error = '';

            this.add(bucket, stream);
        },
        render: function() {
            var self = this;
            return React.DOM.div({className: "connection-manager"},
                                 React.DOM.div({className: "add-connection"},
                                               formRow('bucket', 'Bucket'),
                                               formRow('stream', 'Stream'),
                                               text(this.state.error, 'error'),
                                               button("Add", this.onAdd, "stream-add"),
                                               button("Ping", this.onPing, "stream-ping")),

                                               React.DOM.div({className: "connections"},
                                                             self.state.items.map(function (item) {
                                                                 var key = item.bucket + '/' + item.stream,
                                                                 props = {
                                                                     parent: self,
                                                                     bucket: item.bucket,
                                                                     stream: item.stream,
                                                                     key: key
                                                                 };
                                                                 return mConnection(props);
                                                             })));
        }
    });

    mConnection = React.createClass({
        displayName: 'Connection',
        onRemove: function () {
            this.props.parent.remove(this.props);
        },
        render: function() {
            return React.DOM.div({className: "connection"},
                                 text(this.props.bucket, "bucket-name"),
                                 text(" "),
                                 text(this.props.stream, "stream-name"),
                                 button("X", this.onRemove, "stream-remove"));
        }
    });

    mPage = React.createClass({
        displayName: 'Page',
        render: function() {
            return React.DOM.div({className: "page"},
                                 mLogin(), mConnections());
        }
    });

    function button(text, onClick, className) {
        return React.DOM.button({className: className, onClick: onClick},
                                text);
    }

    function text(label, className) {
        return React.DOM.span({className: className}, label);
    }

    function formRow(id, label, type) {
        type = type || 'text';
        return React.DOM.p({},
                           React.DOM.label({'htmlFor': id}, label),
                           React.DOM.input({
                               name: id,
                               ref: id,
                               type: type
                           }));
    }

    React.renderComponent(mPage(),
                          document.getElementById('connections'));

}

document.addEventListener("DOMContentLoaded", function() {
    'use strict';
    startApp(document, window, $, console, Iorio);
});
