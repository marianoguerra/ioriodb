Iorio DB
========

a stream database.

like a pubsub with history.
or "apache kafka made by couchdb devs"

Iorio DB is a data store that stores streams of events, it's objective is to
allow the following:

* PUT a binary blob (mainly JSON) on a stream, get a sequence number (seqnum) back
* GET last N blobs from a stream
* GET N blobs from a stream starting from a seqnum
* LISTEN to new blobs on one or more streams

  + push (websockets)
  + pull (comet, providing the seqnum you saw last for each stream)

  + catch up to recent events without touching the disk, each streams "remembers" last N events in memory

* provide access control to who can do what on a per stream/bucket level

  + a group of streams is a bucket

* administer user credentials, permissions and sessions
* list streams in a bucket and buckets
* provide eviction policies per stream and/or bucket

  + only keep last N events for a stream
  + only keep X bytes per stream (say 4KB)
  + only keep blobls for the last 7 days

* append only (eviction is done by removing "chunk" files from a stream)

All through a RESTful HTTP API

WARNING
-------

    this is pre alpha software, it will break your hearth (and your data)
    use it only for evaluation and testing

Roadmap
-------

See issues and milestones for details

Test
----

to run the unit tests::

    ./rebar eunit skip_deps=true

to test from he api::

    cd tools/e2e-test

    ./apitest.py -h
    usage: apitest.py [-h] [-H HOST] [-P PORT] [-B BUCKETS] [-S STREAMS] [-s SEED]
                      [-i ITERATIONS]

    Iorio DB API tester

    optional arguments:
      -h, --help            show this help message and exit
      -H HOST, --host HOST  host where ioriodb is running
      -P PORT, --port PORT  port where ioriodb is running
      -B BUCKETS, --buckets BUCKETS
                            number of buckets to use
      -S STREAMS, --streams STREAMS
                            number of streams to use per bucket
      -s SEED, --seed SEED  number of streams to use per bucket
      -i ITERATIONS, --iterations ITERATIONS
                            number of iterations to run

    # 100 iterations for 5 buckets with 5 streams each
    ./apitest.py -i 100

    # 10 clients in parallel, 500 iterations each
    for i in $(seq 10); do ./apitest.py -i 500 &; done

Multinode
---------

**WARNING**: this is still in development

create 4 releases with different config::

    make devrel

start the 4 nodes::

    for d in dev/dev*; do $d/bin/iorio start; done

check that they are running::

    for d in dev/dev*; do $d/bin/iorio ping; done

join 3 nodes to the first one::

    for d in dev/dev{2,3,4}; do $d/bin/iorio-admin cluster join iorio1@127.0.0.1; done

check the status of the cluster::

    dev/dev1/bin/iorio-admin member-status

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

    dev/dev1/bin/iorio-admin cluster plan

it should display the cluster plan, now we can commit the plan::

    dev/dev1/bin/iorio-admin cluster commit

check the status of the cluster again::

    dev/dev1/bin/iorio-admin member-status

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

    for d in dev/dev*; do $d/bin/iorio stop; done

Excercise Handoff
-----------------

first make devrel::

    rm -rf dev
    make devrel

then start one node::

    ./dev/dev1/bin/iorio console

then send it some events so it has some buckets with data::

    tools/e2e-test/apitest.py -P 8098 -B 20 -i 50

now start a second node::

    ./dev/dev2/bin/iorio console

join it to the first one::

    ./dev/dev2/bin/iorio-admin cluster join iorio1@127.0.0.1
    ./dev/dev2/bin/iorio-admin cluster plan
    ./dev/dev2/bin/iorio-admin cluster commit

you should see in the console (if logs set to info/debug) that the data is moving.

you can also watch on the member status how the data moves::

    dev/dev1/bin/iorio-admin member-status

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

    rm -rf dev && make devrel && ./dev/dev1/bin/iorio console

in another one::

    tools/e2e-test/apitest.py -P 8098 -B 20 -i 50 && ./dev/dev2/bin/iorio console

in another one::

    ./dev/dev2/bin/iorio-admin cluster join iorio1@127.0.0.1; \
    ./dev/dev2/bin/iorio-admin cluster plan; \
    ./dev/dev2/bin/iorio-admin cluster commit


License
-------

AGPL v3
