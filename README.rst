Iorio DB
========

Stream Store with HTTP/MQTT Pub/Sub.

Like a pubsub with history or "apache kafka if it was made by couchdb devs"
(Iorio DB is not made by couchdb devs)

Iorio DB is a data store that stores streams of events, it's objective is to
allow the following:

* PUT a binary blob of arbitrary size (mainly JSON) on a stream, get a sequence number (seqnum) back
* GET last N blobs from a stream
* GET N blobs from a stream starting from a seqnum
* LISTEN to new blobs on one or more streams

  + push (WebSockets, Server Sent Events)
  + pull (COMET, providing the seqnum you saw last for each stream)

  + catch up to recent events without touching the disk, each streams "remembers" last N events in memory

* Provide access control to who can do what on a per stream/bucket level
* Administer user credentials, permissions and sessions
* List streams in a bucket and list buckets
* Provide eviction policies per stream and/or bucket

  + Only keep last N events for a stream
  + Only keep X bytes per stream (say 4MB)
  + Only keep blobs for the last 7 days

* Append only (eviction is done by removing "chunk" files from a stream)

All through a RESTful HTTP API

We also support MQTT protocol (only QoS 0 for the moment, QoS 1 planned)

Setup
-----

Start by fetching the dependencies, you can run this anytime you want to remove
the dependencies and fetch the latest versions and apply some fixes we do to
the deps::

    make refetchdeps

After you have all the deps you can simply do::

    make newrel

If you are running for the first time or you don't have config yet do the following::

    mkdir rel/db_config rel/db_data
    cp rel/iorio/etc/{iorio.conf,advanced.config,vm.args} rel/db_config/

Edit rel/db_config/iorio.conf to your needs.

To run a single node::

    make console

API
---

Until we have some docs here is a brief description of the api, check
tools/apitest.py to see how it's used

::

    # put a json document in bucket-id/stream-id
    POST /streams/<bucket-id>/<stream-id>
        X-SESSION: <auth-token>

        <json-body>

    # GET <limit> documents starting from <seqnum> (or last N if seqnum not
    # provided) from bucket-id/stream-id
    GET /streams/<bucket-id>/<stream-id>?[limit=<limit>[&from=<seqnum>]]
        X-SESSION: <auth-token>

    # listen to one or more subscriptions for bucket-id/stream-id
    # (optionally starting at seqnum)
    # this can be also done using websockets, see priv/assets/js/app.js
    # for details
    # NOTE: the auth-token in this case is in the request because websockets
    # don't allow to pass arbitrary headers during handshake
    GET /listen?jwt=<auth-token>[&s=<bucket-id>:<stream-id>[:<seqnum>]]+

    # list buckets (if you have permissions)
    GET /buckets/
        X-SESSION: <auth-token>

    # list streams for <bucket-id> (if you have permissions)
    GET /streams/<bucket-id>
        X-SESSION: <auth-token>

    # get current session details
    GET /sessions
        X-SESSION: <auth-token>

    # login, returns jwt token back that you must use for other requests
    POST /sessions
         {"username": <username>, "password": <password>}

    # create a user (if you have permissions and the user doesn't exist)
    POST /users
         {"username": <username>, "password": <password>}

WARNING
-------

    This is alpha software, it will break your hearth (and your data)
    use it only for evaluation and testing

Test
----

To test from he api::

    ./tools/apitest.py -h

    usage: apitest.py [-h] [-u USERNAME] [-p PASSWORD] [-H HOST] [-P PORT]
                      [-B BUCKETS] [-S STREAMS] [-s SEED] [-i ITERATIONS]
                      [-I INSERTERS] [-L LISTERS] [-R REQUESTERS] [-Q PATCHERS]
                      [--listeners LISTENERS]

    Iorio DB API tester

    optional arguments:
      -h, --help            show this help message and exit
      -u USERNAME, --username USERNAME
                            username used for authentication
      -p PASSWORD, --password PASSWORD
                            password used for authentication
      -H HOST, --host HOST  host where ioriodb is running
      -P PORT, --port PORT  port where ioriodb is running
      -B BUCKETS, --buckets BUCKETS
                            number of buckets to use
      -S STREAMS, --streams STREAMS
                            number of streams to use per bucket
      -s SEED, --seed SEED  number of streams to use per bucket
      -i ITERATIONS, --iterations ITERATIONS
                            number of iterations to run
      -I INSERTERS, --inserters INSERTERS
                            number of threads for inserters to use
      -L LISTERS, --listers LISTERS
                            number of threads for listers to use
      -R REQUESTERS, --requesters REQUESTERS
                            number of threads for requesters to use
      -Q PATCHERS, --patchers PATCHERS
                            number of threads for patchers to use
      --listeners LISTENERS
                            number of threads for listen to events

    # 100 iterations for 5 buckets with 5 streams each, use default credentials
    # use 4 threads for inserters, 1 for listers, 1 for listeners and 2 for
    # requesters
    # that means 4 threads inserting, 2 querying and 1 listing buckets and 
    # steams and 1 listening for new events in a stream

    ./apitest.py -i 100 -I 4 -L 1 -R 2 --listeners 1

To play with the api from the command line::

    $ ./tools/ioriocli.py -h

    usage: ioriocli.py [-h] [--verbose] [-u USERNAME] [-p PASSWORD] [-t TOKEN]
                       [-H HOST] [-P PORT]
                       {post,patch,list-buckets,list-streams,get,listen} ...

    Iorio DB CLI

    positional arguments:
      {post,patch,list-buckets,list-streams,get,listen}
        post                add an event to a stream
        patch               patch last event from a stream
        list-buckets        list buckets
        list-streams        list streams
        get                 get content from a stream
        listen              listen to new content from streams

    optional arguments:
      -h, --help            show this help message and exit
      --verbose, -v
      -u USERNAME, --username USERNAME
                            username used for authentication
      -p PASSWORD, --password PASSWORD
                            password used for authentication
      -t TOKEN, --token TOKEN
                            token from an already authenticated user
      -H HOST, --host HOST  host where ioriodb is running
      -P PORT, --port PORT  port where ioriodb is running

Examples::

    # get last N events from mariano:test
    ./tools/ioriocli.py get mariano test

    # get last N events from mariano:test starting froms seqnum 4
    ./tools/ioriocli.py get mariano test --from 4

    # get event with seqnum 4 from mariano:test
    ./tools/ioriocli.py get mariano test --from 4 --limit 1

    # get last event from mariano:test
    ./tools/ioriocli.py get mariano test --limit 1

    # get last 5 event from mariano:test
    ./tools/ioriocli.py get mariano test --limit 5

    # list buckets
    ./tools/ioriocli.py list-buckets

    # list streams from user mariano
    ./tools/ioriocli.py list-streams mariano

    # listen to mariano:test starting from seqnum 4
    # (will replay events from the past from seqnum 4 if in cache, see note below)
    ./tools/ioriocli.py listen mariano:test:4

    # listen to mariano:test starting from current and listen to
    # mariano:testa from seqnum 10
    # (will replay events from the past from seqnum 4 if in cache)
    ./tools/ioriocli.py listen mariano:testa:10 mariano:test

    # patch last event in mariano:test with the patch specified in the file
    # tools/sample_patch.json (the @ indicates a path), see patch notes below
    ./tools/ioriocli.py patch mariano test @tools/sample_patch.json

    # patch last event in mariano:test with a literal (and invalid) json patch
    ./tools/ioriocli.py patch mariano test '[{}]'

    # patch last event in mariano:test with a literal (and invalid) json patch
    ./tools/ioriocli.py patch mariano test '42'

    # provide wrong password
    ./tools/ioriocli.py -p lala post mariano test @tools/sample.json

    # post a new event on mariano:test with literal json
    ./tools/ioriocli.py post mariano test 42

    # post a new event on mariano:test with literal json
    ./tools/ioriocli.py post mariano test '{"msg": "hi!!"}'

    # post a new event on mariano:test with json fmro a file
    ./tools/ioriocli.py post mariano test @tools/sample.json

    # post a new event on mariano:test with json from a file, provide wrong
    # content type
    ./tools/ioriocli.py post mariano test @tools/sample.json -c "text/plain"

    # patch last event from mariano:test with json from a file, provide wrong
    # content type
    ./tools/ioriocli.py patch mariano test @tools/sample_patch.json -c "text/plain"

Seqnums in listen
.................

When subscribing to events on listen you can specify a seqnum, the current
behaviour is that if you specify a seqnum in the past it will replay from the
closest equal or higher seqnum that the channel has in cache, it won't replay
from disk. The idea of this behaviour is that you can catch up with events that
happened while you weren't listening in the recent past, if you need all the
events from a seqnum onwards you will have to query the stream to be sure you
have all of them.

If you specify a seqnum that is higher than the current one listen will send
you events with smaller seqnums if they happen while you are listening, it's
your choice to adapt the seqnum in the next subscription or to ignore them.

The channel cache contains the last N events for that channel if the events
happen while the channel is alive, periodically a channel will reduce it's
cache if it's inactive to free resources, a channel won't load the last N
events from disk on first creation.

This behaviour may change in the future as we see how it works.

Patch behaviour
...............

Patch only works on streams that already have at least one event, it doesn't
make sense to patch something that's not there, that's why a patch on an
empty stream will fail, you have to handle that case by providing an initial
value and then applying the patch.

Multinode
---------

**WARNING**: this is still in development

create 4 releases with different config::

    make newdevrel

start the 4 nodes::

    make devrel-start

check that they are running::

    make devrel-ping

join 3 nodes to the first one::

    make devrel-join

check the status of the cluster::

    make devrel-status

you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    joining     0.0%      --      'iorio2@127.0.0.1'
    joining     0.0%      --      'iorio3@127.0.0.1'
    joining     0.0%      --      'iorio4@127.0.0.1'
    valid     100.0%      --      'iorio1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:3 / Down:0

it should say that 3 nodes are joining, now check the cluster plan::

    make devrel-cluster-plan

it should display the cluster plan, now we can commit the plan::

    make devrel-cluster-commit

check the status of the cluster again::

    make devrel-status

you could see the vnodes transfering::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      75.0%     25.0%    'iorio1@127.0.0.1'
    valid       9.4%     25.0%    'iorio2@127.0.0.1'
    valid       7.8%     25.0%    'iorio3@127.0.0.1'
    valid       7.8%     25.0%    'iorio4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

at some point you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      25.0%      --      'iorio1@127.0.0.1'
    valid      25.0%      --      'iorio2@127.0.0.1'
    valid      25.0%      --      'iorio3@127.0.0.1'
    valid      25.0%      --      'iorio4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

when you are bored you can stop them::

    make devrel-stop

Excercise Handoff
-----------------

first make devrel::

    make newdevrel

then start one node::

    ./dev/dev1/bin/iorio console

then send it some events so it has some buckets with data::

    tools/apitest.py -P 8098 -B 20 -i 50

now start a second node::

    ./dev/dev2/bin/iorio console

join it to the first one::

    ./dev/dev2/bin/iorio-admin cluster join iorio1@127.0.0.1
    ./dev/dev2/bin/iorio-admin cluster plan
    ./dev/dev2/bin/iorio-admin cluster commit

you should see in the console (if logs set to info/debug) that the data is moving.

you can also watch on the member status how the data moves::

    make devrel-status

as it moves you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      64.1%     50.0%    'iorio1@127.0.0.1'
    valid      35.9%     50.0%    'iorio2@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:2 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

and at the end::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      50.0%      --      'iorio1@127.0.0.1'
    valid      50.0%      --      'iorio2@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:2 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

you can keep adding nodes until you are happy

shortcut for the lazy, in one terminal::

    make newdevrel && ./dev/dev1/bin/iorio console

in another one::

    tools/apitest.py -P 8098 -B 20 -i 50 && ./dev/dev2/bin/iorio console

in another one::

    ./dev/dev2/bin/iorio-admin cluster join iorio1@127.0.0.1; \
    ./dev/dev2/bin/iorio-admin cluster plan; \
    ./dev/dev2/bin/iorio-admin cluster commit

Enabling HTTPS (and wss)
------------------------

First you need to have ssl certificates, let's generate some self signed certificates::

    cd rel
    mkdir ssl
    cd ssl
    openssl genrsa -out key.pem 1024
    openssl req -new -key key.pem -out request.pem

Answer the questions it asks and then::

    openssl x509 -req -days 30 -in request.pem -signkey key.pem -out cert.pem

Now edit iorio.conf, change secure_enabled from no to yes and change the path
to the ssl files if needed, if you followed the commands above you shouldn't
need to change the paths.

Now start ioriodb, on the logs you should see a line like::

    [info] secure api enabled, starting

If you have the cacert file you can provide it also by uncommenting the line
in iorio.conf and setting the correct path, after that you can use ioriodb
by accessing with https://<yourhost>:<secure_port>

Tunning
-------

This section is a draft for now.

You may want to increase some environment variables, just as an example::

    ERL_MAX_PORTS=65536
    ERL_PROCESSES=250000
    ERL_MAX_ETS_TABLES=20000

Also see the following post to get some tips:

http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1

License
-------

`MPL 2 <https://www.mozilla.org/MPL/2.0/>`_
