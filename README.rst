Iorio DB
========

a stream database.

like a pubsub with history.

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

License
-------

AGPL v3
