#!/usr/bin/env python3
import sys
import time
import json
import pprint
import random
import argparse

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
    url = 'http://%s:%d/data/%s/%s' % (host, port, bucket, stream)
    headers = {'content-type': 'application/json'}
    response = requests.post(url, headers=headers, data=data)
    return response

def main():
    args = parse_args()
    log('using seed', args.seed)
    random.seed(args.seed)

    generators = [Generator(i, randint(), args.streams) for i in range(args.buckets)]

    count = 0
    errors = 0
    t1 = time.time()
    for i in range(args.iterations):
        for generator in generators:
            bucket, stream, data = generator.generate()
            data_json = json.dumps(data)
            response = send(args.host, args.port, bucket, stream, data_json)
            count += 1
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


    t2 = time.time()
    t_diff = t2 - t1

    log('sent', count, 'events with', errors, 'errors in', t_diff, 'second',
            count / t_diff, 'events/second')

if __name__ == "__main__":
    main()
