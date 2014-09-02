Iorio DB
========

a stream database.

like a pubsub with history.

.. note:: warning

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

License
-------

AGPL v3
