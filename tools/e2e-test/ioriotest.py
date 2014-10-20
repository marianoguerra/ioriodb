from __future__ import print_function

import json
import iorio
import argparse
import requests

ADMIN_USER = 'admin'
ADMIN_PASS = 'secret'

USER = 'mariano'
USER_PASS = 'secreto'


OK_STATUS = 200
CREATED_STATUS = 201
BAD_REQUEST_STATUS = 400
UNAUTHORIZED_STATUS = 401

def log(*args):
    print(*args)

def get_arg_parser():
    parser = argparse.ArgumentParser(description='Iorio DB API tester')
    parser.add_argument('-u', '--username', default='mariano',
            help='username used for user authentication')
    parser.add_argument('-p', '--password', default='secreto',
            help='password used for user authentication')

    parser.add_argument('-U', '--adminusername', default='admin',
            help='username used for admin authentication')
    parser.add_argument('-a', '--adminpassword', default='secret',
            help='password used for admin authentication')

    parser.add_argument('-H', '--host', default='localhost',
            help='host where ioriodb is running')
    parser.add_argument('-P', '--port', default=8080, type=int,
                       help='port where ioriodb is running')

    return parser

def parse_args():
    parser = get_arg_parser()
    args = parser.parse_args()
    return args

def assert_field(body, field, value):
    assert field in body
    assert body[field] == value

def parse(text):
    try:
        return json.loads(text)
    except Exception as error:
        log(error, "'" + text + "'")
        raise

def test_ping(rsession, args, token):
    url = iorio.format_url(args.host, args.port, 'ping')
    result = iorio.get_json(rsession, url, token)
    assert result.status_code == OK_STATUS
    body = parse(result.text)
    assert "pong" in body
    pong = body["pong"]
    assert isinstance(pong, str)
    assert len(pong) > 0

def test_get_session(session, args, token, username):
    url = iorio.format_url(args.host, args.port, 'sessions')
    result = iorio.get_json(session, url, token)
    assert result.status_code == OK_STATUS
    body = parse(result.text)
    assert_field(body, "username", username)

def create_user(session, args, token, username, password):
    url = iorio.format_url(args.host, args.port, 'users')
    req_body = dict(username=username, password=password)
    return iorio.post_data_json(session, url, req_body, token)

def test_create_user(session, args, token):
    result = create_user(session, args, token, args.username, args.password)
    status = result.status_code
    assert status == CREATED_STATUS or status == BAD_REQUEST_STATUS

    body = parse(result.text)
    if status == CREATED_STATUS:
        assert_field(body, "ok", True)
    else:
        assert_field(body, "type", "user-exists")

def assert_put(result):
    assert result.status_code == CREATED_STATUS
    body = parse(result.text)
    assert "meta" in body
    meta = body["meta"]
    assert "id" in meta
    assert isinstance(meta["id"], int)
    assert "t" in meta
    assert isinstance(meta["t"], int)

def test_can_put_own_stream(session, args, token, user):
    bucket = user
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_put(result)

def test_can_get_own_stream(session, args, token, user):
    bucket = user
    stream = "stream2"
    url = iorio.format_url(args.host, args.port, 'streams', bucket, stream)
    result = iorio.get_json(session, url, token)
    assert result.status_code == OK_STATUS
    body = parse(result.text)
    assert isinstance(body, list)
    assert len(body) == 0

def assert_no_perm(result):
    assert result.status_code == UNAUTHORIZED_STATUS
    body = parse(result.text)
    assert_field(body, "type", "no-perm")

def test_cant_put_other_stream(session, args, token, user, bucket):
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_no_perm(result)

def post(session, token, url, data):
    result = iorio.post_data_json(session, url, data, token)
    if result.text == '':
        log(url, result.status_code)
    body = parse(result.text)
    return result, body

def call_access_stream(session, args, token, bucket, stream, access, username, action):
    url = iorio.format_url(args.host, args.port, 'access', bucket, stream)
    data = dict(permission=access, username=username, action=action)
    return post(session, token, url, data)

def call_access_bucket(session, args, token, bucket, access, username, action):
    url = iorio.format_url(args.host, args.port, 'access', bucket)
    data = dict(permission=access, username=username, action=action)
    return post(session, token, url, data)

def grant_bucket(session, args, token, bucket, access, username):
    return call_access_bucket(session, args, token, bucket, access,
            username, "grant")

def revoke_bucket(session, args, token, bucket, access, username):
    return call_access_bucket(session, args, token, bucket, access,
            username, "revoke")

def grant_stream(session, args, token, bucket, access, stream, username):
    return call_access_stream(session, args, token, bucket, stream, access,
            username, "grant")

def revoke_stream(session, args, token, bucket, stream, access, username):
    return call_access_stream(session, args, token, bucket, stream, access,
            username, "revoke")

def test_call_access_invalid_action(session, args, token, owner, username):
    bucket = owner
    stream = "stream1"
    access = "put"
    result, body = call_access_stream(session, args, token, bucket, stream,
            access, username, "asd")

    assert result.status_code == BAD_REQUEST_STATUS
    assert_field(body, "type", "invalid-field")
    assert_field(body, "field", "action")
    assert_field(body, "value", "asd")

def test_cant_grant_other_bucket(session, args, token, user, user1):
    bucket = user
    result, _body = grant_bucket(session, args, token, bucket, 'put', user1)
    assert_no_perm(result)

def test_can_grant_own_bucket(session, args, token, user, user1):
    bucket = user
    result, body = grant_bucket(session, args, token, bucket, 'put', user1)
    assert result.status_code == OK_STATUS 
    assert_field(body, "ok", True)

def test_can_revoke_own_bucket(session, args, token, user, user1):
    bucket = user
    result, body = revoke_bucket(session, args, token, bucket, 'put', user1)
    assert result.status_code == OK_STATUS
    assert_field(body, "ok", True)

def test_can_put_other_granted_bucket(session, args, token, owner):
    bucket = owner
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_put(result)

def test_cant_put_other_revoked_bucket(session, args, token, owner):
    bucket = owner
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_no_perm(result)

#

def test_cant_grant_other_stream(session, args, token, user, user1):
    bucket = user
    stream = "stream1"
    result, _body = grant_stream(session, args, token, bucket, stream, 'put',
            user1)
    assert_no_perm(result)

def test_can_grant_own_stream(session, args, token, user, user1):
    bucket = user
    stream = "stream1"
    result, body = grant_stream(session, args, token, bucket, 'put', stream,
            user1)
    assert result.status_code == OK_STATUS
    assert_field(body, "ok", True)

def test_can_revoke_own_stream(session, args, token, user, user1):
    bucket = user
    stream = "stream1"
    result, body = revoke_stream(session, args, token, bucket, stream, 'put',
            user1)
    assert result.status_code == OK_STATUS
    assert_field(body, "ok", True)

def test_can_put_other_granted_stream(session, args, token, owner):
    bucket = owner
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_put(result)

def test_cant_put_other_revoked_stream(session, args, token, owner):
    bucket = owner
    stream = "stream1"
    data = dict(i=42, b=True, s="hi", d=42.3, n=None)
    result = send(session, args, token, bucket, stream, data)
    assert_no_perm(result)

def send(session, args, token, bucket, stream, data):
    return iorio.send(session, args.host, args.port, bucket, stream, data,
            token)

def test():
    asession = requests.session()

    args = parse_args()
    ok, admin_token = iorio.authenticate(asession, args.host, args.port,
            args.adminusername, args.adminpassword)

    if not ok:
        log('admin authentication failed')
        return
    else:
        log("token", admin_token)

    test_ping(asession, args, admin_token)
    test_get_session(asession, args, admin_token, args.adminusername)
    test_create_user(asession, args, admin_token)
    # to check user exists error
    test_create_user(asession, args, admin_token)

    usession = requests.session()
    u1session = requests.session()

    user = args.username
    user1 = user + '1'

    ok, user_token = iorio.authenticate(usession, args.host, args.port, user,
        args.password)

    if not ok:
        log('user authentication failed')
        return
    else:
        log("token", user_token)

    create_user(asession, args, admin_token, user1, args.password)
    ok, user1_token = iorio.authenticate(u1session, args.host, args.port,
            user1, args.password)

    if not ok:
        log('user1 authentication failed')
        return
    else:
        log("token", user1_token)

    test_ping(usession, args, user_token)
    test_get_session(usession, args, user_token, user)

    test_can_put_own_stream(usession, args, user_token, user)
    test_can_get_own_stream(usession, args, user_token, user)
    test_cant_put_other_stream(usession, args, user_token, user, user1)

    test_can_grant_own_stream(usession, args, user_token, user, user1)
    test_cant_grant_other_stream(u1session, args, user1_token, user, user1)

    test_can_put_other_granted_stream(u1session, args, user1_token, user)
    test_can_revoke_own_stream(usession, args, user_token, user, user1)
    test_cant_put_other_revoked_stream(u1session, args, user1_token, user)

    test_call_access_invalid_action(usession, args, user_token, user, user1)
    test_cant_put_other_stream(u1session, args, user1_token, user1, user)

    #

    test_can_grant_own_bucket(usession, args, user_token, user, user1)
    test_cant_grant_other_bucket(u1session, args, user1_token, user, user1)

    test_can_put_other_granted_bucket(u1session, args, user1_token, user)
    test_can_revoke_own_bucket(usession, args, user_token, user, user1)
    test_cant_put_other_revoked_bucket(u1session, args, user1_token, user)

    test_cant_put_other_stream(u1session, args, user1_token, user1, user)

if __name__ == '__main__':
    test()
