#!/usr/bin/env python3
'''ioriodb CLI client to interact with the api from the command line'''
from __future__ import print_function
import json
import pprint
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
    p_get.add_argument('-f', '--from', default=None, type=int,
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

def get_auth_token(rsession, args):
    '''if token is set return it otherwise try to authenticate'''
    if args.token:
        return True, args.token
    else:
        return iorio.authenticate(rsession, args.host, args.port,
                args.username, args.password)

def show_response(resp):
    '''show content of request response'''
    print("Status:", resp.status_code)

    if not resp.text:
        print("No Response Body")
        return

    try:
        json_body = json.loads(resp.text)
        print("JSON Response:")
        pprint.pprint(json_body)
    except ValueError:
        print("Raw Response:", resp.text)

def do_when_authenticated(args, fun, rsession=None):
    '''if auth works run fun'''
    if rsession is None:
        rsession = iorio.new_session()

    auth_ok, auth_data = get_auth_token(rsession, args)

    if auth_ok:
        token = auth_data
        response = fun(rsession, token)
        show_response(response)
    else:
        auth_response = auth_data
        print("Auth Failed")
        show_response(auth_response)

def post_or_patch(args, name):
    '''avoid duplication'''
    host = args.host
    port = args.port

    bucket = args.bucket
    stream = args.stream

    content_type = args.content_type
    data_raw = args.data
    data = parse_data_from_raw(data_raw)

    def fun(rsession, token):
        '''fun that does the work'''
        function = getattr(iorio, name)
        return function(rsession, host, port, bucket, stream, data,
                token, content_type)

    do_when_authenticated(args, fun)

def handle_post_event(args):
    '''post a new event'''
    post_or_patch(args, 'send')

def handle_patch_event(args):
    '''patch a new event'''
    post_or_patch(args, 'patch')

def handle_get_events(args):
    '''get events'''
    host = args.host
    port = args.port

    bucket = args.bucket
    stream = args.stream

    limit = args.limit

    def fun(rsession, token):
        '''fun that does the work'''
        return iorio.query(rsession, host, port, bucket, stream,
                limit, token)

    do_when_authenticated(args, fun)

def handle_list_streams(args):
    '''get events'''
    host = args.host
    port = args.port
    bucket = args.bucket

    def fun(rsession, token):
        '''fun that does the work'''
        return iorio.list_streams(rsession, host, port, bucket, token)

    do_when_authenticated(args, fun)

def handle_list_buckets(args):
    '''get events'''
    host = args.host
    port = args.port

    def fun(rsession, token):
        '''fun that does the work'''
        return iorio.list_buckets(rsession, host, port, token)

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
    host = args.host
    port = args.port

    raw_subs = args.subscriptions
    subs = iorio.Subscriptions()

    for sub in raw_subs:
        ok, result = parse_subscription(sub)
        if not ok:
            print(result)
            return

        bucket, stream, count = result
        subs.add(bucket, stream, count)

    def fun(rsession, token):
        '''fun that does the work'''
        while True:
            current_subs = subs.to_list()
            print('listening', ' '.join(current_subs))
            response = iorio.listen(rsession, host, port, current_subs, token)
            show_response(response)
            print()
            if response.status_code == 200:
                body = json.loads(response.text)
                subs.update_seqnums(body)

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
