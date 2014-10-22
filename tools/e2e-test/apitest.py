#!/usr/bin/env python3
from __future__ import print_function
import sys
import time
import json
import pprint
import random
import argparse
import threading
import multiprocessing

from iorio import *

import faker
import requests

class Generator(object):
    def __init__(self, index, seed, stream_count):
        self.index = index
        self.seed = seed
        self.faker = faker.Factory.create()
        self.faker.seed(seed)
        self.bucket = self.faker.domain_word()
        self.streams = [self.faker.domain_word() for _ in range(stream_count)]
        log('initialized generator', self.bucket, 'buckets:',
                ', '.join(self.streams))

    def generate(self):
        data = dict(bool=self.faker.null_boolean(),
                user=self.faker.user_name(),
                date=self.faker.date_time().isoformat(),
                lat=float(self.faker.latitude()),
                lng=float(self.faker.longitude()))

        stream = self.streams.pop(0)
        self.streams.append(stream)

        return self.bucket, stream, data

def log(*args):
    print(*args)

def randint():
    return random.randint(0, 500000)

def get_arg_parser():
    default_seed = randint()
    cpu_count = multiprocessing.cpu_count()
    parser = argparse.ArgumentParser(description='Iorio DB API tester')
    parser.add_argument('-u', '--username', default='admin',
            help='username used for authentication')
    parser.add_argument('-p', '--password', default='secret',
            help='password used for authentication')
    parser.add_argument('-H', '--host', default='localhost',
            help='host where ioriodb is running')
    parser.add_argument('-P', '--port', default=8080, type=int,
                       help='port where ioriodb is running')
    parser.add_argument('-B', '--buckets', default=5, type=int,
                       help='number of buckets to use')
    parser.add_argument('-S', '--streams', default=5, type=int,
                       help='number of streams to use per bucket')
    parser.add_argument('-s', '--seed', default=default_seed, type=int,
                       help='number of streams to use per bucket')
    parser.add_argument('-i', '--iterations', default=10, type=int,
                       help='number of iterations to run')
    parser.add_argument('-t', '--threads', default=cpu_count, type=int,
                       help='number of threads to use')

    return parser

def parse_args():
    parser = get_arg_parser()
    args = parser.parse_args()
    return args

class BaseRequester(threading.Thread):

    def __init__(self, host, port, token=None):
        threading.Thread.__init__(self)
        self.daemon = True
        self.host = host
        self.port = port
        self.stop = False
        self.errors = 0
        self.query_count = 0
        self.max_sleep_secs = 5
        self.token = token
        self.rsession = requests.Session()

    def on_no_json_error(self, response, ctx, time_ms):
        log('response is not json:', response.text, response.status_code, ctx.__dict__)

    def on_error(self, response, body, ctx, time_ms):
        log('error response')
        pprint.pprint(body)

    def on_success(self, response, body, ctx, time_ms):
        pass

    def request(self):
        raise NotImplementedError()

    def run(self):
        while not self.stop:
            t_1 = time.time()
            response, ctx = self.request()
            t_2 = time.time()
            time_ms = t_2 - t_1
            self.query_count += 1

            is_error = (response.status_code != 200)
            try:
                if is_error:
                    self.errors += 1
                body = json.loads(response.text)

                if is_error:
                    self.on_error(response, body, ctx, time_ms)
                else:
                    self.on_success(response, body, ctx, time_ms)

            except ValueError:
                self.on_no_json_error(response, ctx, time_ms)

            time.sleep(random.randint(0, self.max_sleep_secs))

class Obj(object):
    def __init__(self, **args):
        self.__dict__ = args

class QueryRequester(BaseRequester):

    def __init__(self, generators, host, port, token=None):
        BaseRequester.__init__(self, host, port, token)
        self.generators = generators
        self.item_count = 0

    def on_success(self, response, body, ctx, time_ms):
        BaseRequester.on_success(self, response, body, ctx, time_ms)
        self.item_count += ctx.limit
        log('GET %s/%s %d => %d (%f ms)' % (ctx.bucket, ctx.stream, ctx.limit,
            len(response.text), time_ms))

    def on_error(self, response, body, ctx, time_ms):
        BaseRequester.on_error(self, response, body, ctx, time_ms)
        log("Error %s/%s (%d)" % (ctx.stream, ctx.bucket, ctx.limit))

    def request(self):
        generator = random.choice(self.generators)
        bucket = generator.bucket
        stream = random.choice(generator.streams)
        limit = random.randint(0, 100)

        response = query(self.rsession, self.host, self.port, bucket, stream,
                limit, self.token)

        return response, Obj(bucket=bucket, stream=stream, limit=limit)

    def format_summary(self):
        return 'queried %d times for %d items with %d read errors' % (
            self.query_count, self.item_count, self.errors)

class BucketLister(BaseRequester):
    def __init__(self, generators, host, port, token=None):
        BaseRequester.__init__(self, host, port, token)
        self.generators = generators
        self.max_sleep_secs = 10
        self.list_requests = 0
        self.lists_requests = 0

    def on_success(self, response, body, ctx, time_ms):
        BaseRequester.on_success(self, response, body, ctx, time_ms)
        items = ", ".join(body["data"])
        if ctx.list_buckets:
            log("GET /buckets/ => %s (%f ms)" % (items, time_ms))
        else:
            log("GET /streams/%s => %s (%f ms)" % (ctx.bucket, items, time_ms))

    def on_error(self, response, body, ctx, time_ms):
        BaseRequester.on_error(self, response, body, ctx, time_ms)
        if ctx.list_buckets:
            log("Error listing buckets")
        else:
            log("Error listing streams %s" % ctx.bucket)

    def list_buckets(self):
        url = format_url(self.host, self.port, "buckets")
        return get_json(self.rsession, url, self.token)

    def list_bucket(self, bucket):
        url = format_url(self.host, self.port, "streams", bucket)
        return get_json(self.rsession, url, self.token)

    def request(self):
        if random.randint(0, 3) == 2:
            self.lists_requests += 1
            return self.list_buckets(), Obj(list_buckets=True, bucket=None)
        else:
            generator = random.choice(self.generators)
            bucket = generator.bucket
            self.list_requests += 1
            return self.list_bucket(bucket), Obj(list_buckets=False, bucket=bucket)

    def format_summary(self):
        return "listed buckets %d times, listed streams %d times with %d errors" % (
                self.lists_requests, self.list_requests, self.errors)

class Inserter(threading.Thread):

    def __init__(self, token, generators, args):
        threading.Thread.__init__(self)
        self.token = token
        self.generators = generators
        self.args = args
        self.count = 0
        self.errors = 0
        self.t_diff = 0
        self.rsession = requests.Session()

    def run(self):
        args = self.args
        t_1 = time.time()

        loop_count = int(args.iterations / len(self.generators))
        for _ in range(loop_count):
            for generator in self.generators:
                bucket, stream, data = generator.generate()
                try:
                    response = send(self.rsession, args.host, args.port, bucket,
                            stream, data, self.token)
                except Exception as error:
                    self.errors += 1
                    log('error sending data', error)
                    continue

                self.count += 1

                if self.count % 500 == 0:
                    log('%d inserts' % self.count)

                if response.status_code != 201:
                    self.errors += 1
                    log('error sending data', response.status_code)
                    pprint.pprint(data)
                    try:
                        body = json.loads(response.text)
                        log('error response')
                        pprint.pprint(body)
                    except ValueError:
                        log('response is not json:', bucket, stream,
                                response.status_code, response.text)


        t_2 = time.time()
        self.t_diff = t_2 - t_1

    def format_summary(self):
        return 'sent %d with %d write errors in %f s, %f events/s' % (
                self.count, self.errors, self.t_diff,
                (self.count / self.t_diff))

def main():
    args = parse_args()
    log('using seed', args.seed)
    random.seed(args.seed)

    ok, token = authenticate(requests, args.host, args.port, args.username,
            args.password)

    if not ok:
        log('authentication failed')
        return
    else:
        log("token", token)

    generators = [Generator(i, randint(), args.streams) \
            for i in range(args.buckets)]

    inserters = []
    requesters = []
    listers = []
    for _ in range(args.threads):
        inserter = Inserter(token, generators, args)
        inserter.start()
        inserters.append(inserter)

        requester = QueryRequester(generators, args.host, args.port, token)
        requester.start()
        requesters.append(requester)

        bucket_lister = BucketLister(generators, args.host, args.port, token)
        bucket_lister.start()
        listers.append(bucket_lister)

    for inserter in inserters:
        inserter.join()
        log(inserter.format_summary())


    for requester in requesters:
        requester.stop = True
        requester.join()
        log(requester.format_summary())

    for bucket_lister in listers:
        bucket_lister.stop = True
        bucket_lister.join()
        log(bucket_lister.format_summary())

if __name__ == "__main__":
    main()
