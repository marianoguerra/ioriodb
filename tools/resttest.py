#!/usr/bin/env python3
'''module to test all rest api behavior, for correct paths and also handling
errors, check returned status code, content type and body'''
from __future__ import print_function
import sys
import time
import argparse

import iorio

MT_JSON = 'application/json'
MT_XML = 'application/xml'

def error(*args):
    '''poor man's error log'''
    print(*args, file=sys.stderr)

def log(*args):
    '''poor man's log'''
    print(*args)

def get_arg_parser():
    '''generate cli arg parser'''
    tstamp = str(int(time.time()))
    parser = argparse.ArgumentParser(description='Iorio DB API checker')

    parser.add_argument('-u', '--username', default='admin',
            help='username used for authentication')
    parser.add_argument('-p', '--password', default='secret',
            help='password used for authentication')

    parser.add_argument('-U', '--tempuser', default='temp' + tstamp,
            help='username used for tests')
    parser.add_argument('-Q', '--temppass', default='secret',
            help='password used for tests')

    parser.add_argument('-H', '--host', default='localhost',
            help='host where ioriodb is running')
    parser.add_argument('-P', '--port', default=8080, type=int,
                       help='port where ioriodb is running')

    return parser

def parse_args():
    '''parse cli args'''
    parser = get_arg_parser()
    args = parser.parse_args()

    return args

def expect(obj, field_name, value):
    '''check that field has expected value, print error otherwise'''
    if isinstance(obj, dict):
        actual = obj.get(field_name)
    elif obj is not None:
        actual = getattr(obj, field_name)
    else:
        actual = None

    if actual != value:
        error('Expected', field_name, 'to be', value, 'but got', actual)

def expect_json_error(resp, status, error_type):
    '''helper to check many things in one call'''
    expect(resp, 'status', status)
    expect(resp, 'content_type', MT_JSON)
    expect(resp.body, 'type', error_type)

def test_create_user_bad_body(conn, username, password):
    '''test creating a user with incorrect body'''
    log('creating user with incorrect content type', username)
    body = conn.make_body(username=username, password=password)
    resp = conn.raw_create_user(body, MT_XML)
    expect(resp, 'status', 415)
    expect(resp, 'body', None)

    log('creating user with invalid json body', username)
    body = conn.make_body(username=username, password=password)[:-1]
    resp = conn.raw_create_user(body, MT_JSON)
    expect_json_error(resp, 400, 'invalid-body')

def test_create_user_missing_field(conn, username, password):
    '''test creating a user without a required field'''
    log('creating user with missing password', username)
    resp = conn.raw_create_user(conn.make_body(username=username))
    expect_json_error(resp, 400, 'no-pass')

    log('creating user with missing username', username)
    resp = conn.raw_create_user(conn.make_body(password=password))
    expect_json_error(resp, 400, 'no-user')

    log('creating user with empty object body', username)
    resp = conn.raw_create_user(conn.make_body())
    expect_json_error(resp, 400, 'no-user-and-pass')

def test_create_user(conn, username, password):
    '''test creating a user'''
    log('creating user', username)
    resp = conn.create_user(username, password)
    expect(resp, 'status', 201)
    expect(resp, 'content_type', MT_JSON)
    expect(resp.body, 'ok', True)

def test_create_user_again(conn, username, password):
    '''test creating a user for a second time'''
    log('creating user again', username)
    resp = conn.create_user(username, password)
    expect_json_error(resp, 400, 'user-exists')

def test_create_user_invalid_username(conn, username, password):
    '''test creating a user with invalid username'''
    log('creating user invalid username', username)
    resp = conn.create_user(username + ' %$&/(', password)
    expect_json_error(resp, 400, 'illegal-username')

def test_all_create_user(conn, args):
    '''test all create user things'''
    log('Testing Create User')
    test_create_user_missing_field(conn, args.tempuser, args.temppass)
    log()
    test_create_user_bad_body(conn, args.tempuser, args.temppass)
    log()
    test_create_user(conn, args.tempuser, args.temppass)
    log()
    test_create_user_again(conn, args.tempuser, args.temppass)
    log()
    test_create_user_invalid_username(conn, args.tempuser, args.temppass)

def test_send_event(conn, bucket, stream):
    '''test sending a correct event'''
    log('send event', bucket, stream)
    resp = conn.send(bucket, stream, dict(name='bob', age=29, sponge=True))
    expect(resp, 'status', 201)
    expect(resp, 'content_type', MT_JSON)
    if resp.body:
        expect(resp.body.get("meta"), 'id', 1)
    else:
        error('Expected body to be set')

def test_send_event_no_auth(conn, bucket, stream):
    '''test sending a correct event'''
    log('send event', bucket, stream)
    resp = conn.send(bucket, stream, dict(name='bob', age=29, sponge=True))
    expect(resp, 'status', 401)
    expect(resp, 'content_type', MT_JSON)
    expect(resp.body, 'type', 'no-perm')

def test_all_send_event(aconn, uconn, args):
    '''test all send event things'''
    log('Testing Send Event')
    test_send_event(aconn, args.username, args.tempuser)
    # admin can post into user bucket
    test_send_event(aconn, args.tempuser, args.username)
    test_send_event(uconn, args.tempuser, args.tempuser)

    test_send_event_no_auth(uconn, 'foo', 'bar')

def main():
    '''main test entry point'''
    args = parse_args()
    aconn = iorio.Connection(args.host, args.port)
    uconn = iorio.Connection(args.host, args.port)
    log("authenticating", args.username)
    auth_ok, _result = aconn.authenticate(args.username, args.password)

    assert auth_ok

    test_all_create_user(aconn, args)
    auth_ok, _result = uconn.authenticate(args.tempuser, args.temppass)

    assert auth_ok
    test_all_send_event(aconn, uconn, args)


if __name__ == '__main__':
    main()
