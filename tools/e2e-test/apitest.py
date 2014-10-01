#!/usr/bin/env python3
from __future__ import print_function
import sys
import time
import json
import pprint
import random
import argparse
import threading

import faker
import requests

class Generator(object):
    def __init__(self, index, seed, stream_count):
        self.index = index
        self.seed = seed
        self.faker = faker.Factory.create()
        self.faker.seed(seed)
        self.bucket = self.faker.domain_word()
        self.streams = [self.faker.domain_word() for i in range(stream_count)]
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
    parser = argparse.ArgumentParser(description='Iorio DB API tester')
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

    return parser

def parse_args():
    parser = get_arg_parser()
    args = parser.parse_args()
    return args

def send(host, port, bucket, stream, data):
    url = 'http://%s:%d/streams/%s/%s' % (host, port, bucket, stream)
    headers = {'content-type': 'application/json'}
    response = requests.post(url, headers=headers, data=data)
    return response

def get_json(url):
    headers = {'content-type': 'application/json'}
    response = requests.get(url, headers=headers)
    return response

def query(host, port, bucket, stream, limit):
    url = 'http://%s:%d/streams/%s/%s?limit=%s' % (host, port, bucket, stream, limit)
    return get_json(url)


class BaseRequester(threading.Thread):

    def __init__(self, host, port):
        threading.Thread.__init__(self)
        self.daemon = True
        self.host = host
        self.port = port
        self.stop = False
        self.errors = 0
        self.query_count = 0
        self.max_sleep_secs = 5

    def on_no_json_error(self, response, ctx, time_ms):
        log('response is not json:', response.text)

    def on_error(self, response, body, ctx, time_ms):
        log('error response')
        pprint.pprint(body)

    def on_success(self, response, body, ctx, time_ms):
        pass

    def request(self):
        raise NotImplementedError()

    def run(self):
        while not self.stop:
            t1 = time.time()
            response, ctx = self.request()
            t2 = time.time()
            time_ms = t2 - t1
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

    def __init__(self, generators, host, port):
        BaseRequester.__init__(self, host, port)
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

        response = query(self.host, self.port, bucket, stream, limit)


        return response, Obj(bucket=bucket, stream=stream, limit=limit)

class BucketLister(BaseRequester):
    def __init__(self, generators, host, port):
        BaseRequester.__init__(self, host, port)
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
            log("GET /buckets/%s => %s (%f ms)" % (ctx.bucket, items, time_ms))

    def on_error(self, response, body, ctx, time_ms):
        BaseRequester.on_error(self, response, body, ctx, time_ms)
        if ctx.list_buckets:
            log("Error listing buckets")
        else:
            log("Error listing bucket %s" % ctx.bucket)

    def list_buckets(self):
        url = 'http://%s:%d/buckets/' % (self.host, self.port)
        return get_json(url)

    def list_bucket(self, bucket):
        url = 'http://%s:%d/buckets/%s' % (self.host, self.port, bucket)
        return get_json(url)

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

def main():
    args = parse_args()
    log('using seed', args.seed)
    random.seed(args.seed)

    generators = [Generator(i, randint(), args.streams) for i in range(args.buckets)]
    requester = QueryRequester(generators, args.host, args.port)
    requester.start()

    bucket_lister = BucketLister(generators, args.host, args.port)
    bucket_lister.start()

    count = 0
    errors = 0
    t1 = time.time()
    for i in range(args.iterations):
        for generator in generators:
            bucket, stream, data = generator.generate()
            data_json = json.dumps(data)
            response = send(args.host, args.port, bucket, stream, data_json)
            count += 1

            if count % 500 == 0:
                log('%d inserts' % count)

            if response.status_code != 200:
                errors += 1
                log('error sending data', response.status_code)
                pprint.pprint(data)
                try:
                    body = json.loads(response.text)
                    log('error response')
                    pprint.pprint(body)
                except ValueError:
                    log('response is not json:', response.text)


    requester.stop = True
    t2 = time.time()
    t_diff = t2 - t1

    log('sent', count, 'events with', errors, 'write errors and queried',
            requester.query_count, 'times for', requester.item_count, 'items with',
            requester.errors, 'read errors in', t_diff, 'second',
            count / t_diff, 'events/second')

    log(bucket_lister.format_summary())

if __name__ == "__main__":
    main()
