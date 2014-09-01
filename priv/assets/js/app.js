/*globals $, document, window, console, Iorio, React, JsonHuman*/
function startApp(document, window, $, console, Iorio) {
    'use strict';
    var iorio = new Iorio('ws://localhost:8080/listen', {}),
        output = document.getElementById('traffic'),
        mConnections, mConnection, mNewConnection, mConnectionManager,
        conns;

    function addTraffic(data) {
        var node = JsonHuman.format(data);
        output.appendChild(node);
    }

    iorio.onSend.add(addTraffic);
    iorio.onData.add(addTraffic);

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

    function button(text, onClick, className) {
        return React.DOM.button({className: className, onClick: onClick},
                                text);
    }

    function text(label, className) {
        return React.DOM.span({className: className}, label);
    }

    function formRow(id, label, value) {
        return React.DOM.p({},
                           React.DOM.label({'htmlFor': id}, label),
                           React.DOM.input({name: id, ref: id, value: value}));
    }

    React.renderComponent(mConnections(),
                          document.getElementById('connections'));

}

document.addEventListener("DOMContentLoaded", function() {
    'use strict';
    startApp(document, window, $, console, Iorio);
});
