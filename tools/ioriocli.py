#!/usr/bin/env python3
'''ioriodb CLI client to interact with the api from the command line'''
from __future__ import print_function
import time
import json
import argparse

import iorio

def get_arg_parser():
    '''build the cli arg parser'''
    parser = argparse.ArgumentParser(description='Iorio DB CLI')
    parser.add_argument('--verbose', '-v', action='count')
    parser.add_argument('-u', '--username', default='admin',
                        help='username used for authentication')
    parser.add_argument('-p', '--password', default='secret',
                        help='password used for authentication')
    parser.add_argument('-t', '--token', default=None,
                        help='token from an already authenticated user')

    parser.add_argument('-H', '--host', default='localhost',
                        help='host where ioriodb is running')
    parser.add_argument('-P', '--port', default=8080, type=int,
                        help='port where ioriodb is running')

    parser.add_argument('--human', action='store_true', default=False)

    subparsers = parser.add_subparsers()
    p_post = subparsers.add_parser('post', help='add an event to a stream')
    p_patch = subparsers.add_parser('patch',
            help='patch last event from a stream')
    p_list_buckets = subparsers.add_parser('list-buckets', help='list buckets')
    p_list_streams = subparsers.add_parser('list-streams', help='list streams')
    p_get = subparsers.add_parser('get', help='get content from a stream')
    p_listen = subparsers.add_parser('listen',
            help='listen to new content from streams')

    #p_admin = subparsers.add_parser('admin', help='admin tasks')

    p_post.set_defaults(action='post')
    p_post.add_argument('bucket', help='bucket name')
    p_post.add_argument('stream', help='stream name')
    p_post.add_argument('-c', '--content-type', default='application/json',
                        help='content-type for the request')
    p_post.add_argument('data', help='literal JSON data or if starts with @ ' +
            'path to a file with JSON data')

    p_patch.set_defaults(action='patch')
    p_patch.add_argument('bucket', help='bucket name')
    p_patch.add_argument('stream', help='stream name')
    p_patch.add_argument('-c', '--content-type',
                        default='application/json-patch+json',
                        help='content-type for the request')
    p_patch.add_argument('data', help='literal JSON data or if starts with @ ' +
            'path to a file with JSON data')

    p_get.set_defaults(action='get')
    p_get.add_argument('bucket', help='bucket name')
    p_get.add_argument('stream', help='stream name')
    p_get.add_argument('-l', '--limit', default=10, type=int,
                        help='amount of items to retrieve')
    p_get.add_argument('-f', '--from', default=None, type=int, dest='fromsn',
                        help='sequence number to start from')

    p_list_buckets.set_defaults(action='list-buckets')

    p_list_streams.set_defaults(action='list-streams')
    p_list_streams.add_argument('bucket', help='bucket name')

    p_listen.set_defaults(action='listen')
    p_listen.add_argument('subscriptions', nargs='+',
        help="subscription descriptiors (bucket:stream or bucket:stream:from)")

    return parser

def parse_args():
    '''parse arguments and return them'''
    parser = get_arg_parser()
    args = parser.parse_args()
    return args

def parse_data_from_raw(data_raw):
    '''parse data from literal, if it starts wit @ parse content from file'''
    if data_raw.startswith('@'):
        return json.load(open(data_raw[1:]))
    else:
        return json.loads(data_raw)

def do_when_authenticated(args, fun, conn=None):
    '''if auth works run fun'''
    if conn is None:
        conn = iorio.Connection(args.host, args.port)

    auth_t1 = time.time()
    auth_ok, auth_resp = conn.authenticate(args.username, args.password)
    auth_t2 = time.time()

    if args.verbose and args.verbose > 1:
        print("Auth request time", (auth_t2 - auth_t1) * 1000, "ms")

    if auth_ok:
        req_t1 = time.time()
        response = fun(conn)
        req_t2 = time.time()

        if args.verbose and args.verbose > 1:
            print("Request time", (req_t2 - req_t1) * 1000, "ms")

        print(response)
    else:
        print("Auth Failed")
        print(auth_resp)

def post_or_patch(args, name):
    '''avoid duplication'''
    bucket = args.bucket
    stream = args.stream

    content_type = args.content_type
    data_raw = args.data
    data = parse_data_from_raw(data_raw)

    def fun(conn):
        '''fun that does the work'''
        function = getattr(conn, name)
        return function(bucket, stream, data, content_type)

    do_when_authenticated(args, fun)

def handle_post_event(args):
    '''post a new event'''
    post_or_patch(args, 'send')

def handle_patch_event(args):
    '''patch a new event'''
    post_or_patch(args, 'send_patch')

def handle_get_events(args):
    '''get events'''
    bucket = args.bucket
    stream = args.stream

    limit = args.limit
    fromsn = args.fromsn

    def fun(conn):
        '''fun that does the work'''
        return conn.query(bucket, stream, fromsn, limit)

    do_when_authenticated(args, fun)

def handle_list_streams(args):
    '''get events'''
    bucket = args.bucket

    def fun(conn):
        '''fun that does the work'''
        return conn.list_streams(bucket)

    do_when_authenticated(args, fun)

def handle_list_buckets(args):
    '''get events'''
    def fun(conn):
        '''fun that does the work'''
        return conn.list_buckets()

    do_when_authenticated(args, fun)

def parse_subscription(sub):
    '''parse a subscription in notation bucket:stream[:from]'''

    parts = sub.split(':')
    parts_count = len(parts)

    if parts_count == 2:
        return True, parts + [None]
    elif parts_count == 3:
        try:
            seqnum = int(parts[2])
            return True, [parts[0], parts[1], seqnum]
        except ValueError:
            return (False, "expected subscription to have format " +
                    "bucket:stream:from where from is a number, got %s" % sub)
    else:
        return (False, "expected subscription to have format " +
                "bucket:stream[:from], got %s" % sub)

def handle_listen(args):
    '''listen to events in subscriptions'''
    raw_subs = args.subscriptions
    subs = iorio.Subscriptions()

    for sub in raw_subs:
        ok, result = parse_subscription(sub)
        if not ok:
            print(result)
            return

        bucket, stream, count = result
        subs.add(bucket, stream, count)

    def fun(conn):
        '''fun that does the work'''
        while True:
            current_subs = subs.to_list()
            print('listening', ' '.join(current_subs))
            response = conn.listen(current_subs)
            print(response)
            print()
            if response.status == 200:
                subs.update_seqnums(response.body)

    do_when_authenticated(args, fun)


HANDLERS = {
        'post': handle_post_event,
        'patch': handle_patch_event,
        'get': handle_get_events,
        'listen': handle_listen,
        'list-buckets': handle_list_buckets,
        'list-streams': handle_list_streams
}

def main():
    '''cli entry point'''
    args = parse_args()
    handler = HANDLERS[args.action]
    handler(args)

if __name__ == '__main__':
    main()
