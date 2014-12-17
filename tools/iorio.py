'''iorio python api'''
from __future__ import print_function

import json
import time
import pprint
import urllib
import requests

class Response(object):

    def __init__(self, status, raw_body, body, content_type, source):
        self.status = status
        self.raw_body = raw_body
        self.body = body
        self.source = source
        self.content_type = content_type

    @classmethod
    def from_response(cls, response):
        status = response.status_code
        raw_body = response.text
        body = None
        content_type = response.headers.get("Content-Type")

        try:
            body = json.loads(raw_body)
        except ValueError:
            pass

        return cls(status, raw_body, body, content_type, response)

class Connection(object):
    MT_JSON = 'application/json'
    MT_JSON_PATCH = 'application/json-patch+json'

    def __init__(self, host, port, secure=False, session_header_name='x-session'):
        self.host = host
        self.port = port
        self.session = requests.Session()
        self.secure = secure
        self.token = None
        self.session_header_name = session_header_name

    def format_url(self, paths, query_params=None):
        if query_params:
            params = "?" + "&".join(("%s=%s" % (key, str(val))) for key, val in query_params.items())
        else:
            params = ""

        path = "/".join(str(item) for item in paths)
        protocol = 'https' if self.secure else 'http'
        return '%s://%s:%d/%s%s' % (protocol, self.host, self.port, path, params)

    def make_headers(self, content_type=MT_JSON):
        headers = {'content-type': content_type}

        if self.token:
            headers[self.session_header_name] = self.token

        return headers

    def query_req(self, method, paths, query_params, content_type):
        headers = self.make_headers(content_type)
        url = self.format_url(paths, query_params)
        req_method = getattr(self.session, method)
        response = req_method(url, headers=headers)
        return Response.from_response(response)

    def data_req(self, method, data, paths, query_params, content_type):
        headers = self.make_headers(content_type)
        url = self.format_url(paths, query_params)
        req_method = getattr(self.session, method)
        response = req_method(url, headers=headers, data=data)
        return Response.from_response(response)

    def post(self, data, paths, query_params=None, content_type=MT_JSON):
        return self.data_req('post', data, paths, query_params, content_type)

    def put(self, data, paths, query_params=None, content_type=MT_JSON):
        return self.data_req('put', data, paths, query_params, content_type)

    def patch(self, data, paths, query_params=None, content_type=MT_JSON_PATCH):
        return self.data_req('patch', data, paths, query_params, content_type)

    def get(self, paths, query_params=None, content_type=MT_JSON):
        return self.query_req('get', paths, query_params, content_type)

    def delete(self, paths, query_params=None, content_type=MT_JSON):
        return self.query_req('delete', paths, query_params, content_type)

    def make_body(self, **fields):
        return json.dumps(fields)

    def send(self, bucket, stream, data):
        return self.post(json.dumps(data), ['streams', bucket, stream])

    def send_patch(self, bucket, stream, data):
        return self.patch(json.dumps(data), ['streams', bucket, stream])

    def list_buckets(self):
        return self.get(['buckets'])

    def list_streams(self, bucket):
        return self.get(['streams', bucket])

    def listen(self, subs, content_type=MT_JSON):
        headers = self.make_headers(content_type)
        token = urllib.parse.quote(self.token)
        url_base = self.format_url(["listen"], {'jwt': token})
        url = url_base + '&' + '&'.join(['s=%s' % sub for sub in subs]) + '&' + str(time.time())

        response = self.session.get(url, headers=headers)
        return Response.from_response(response)

    def query(self, bucket, stream, fromsn, limit):
        params = {'limit': limit}
        if fromsn is not None:
            params['from'] = fromsn

        return self.get(['streams', bucket, stream], params)

    def authenticate(self, username, password):
        req_body = self.make_body(username=username, password=password)
        response = self.post(req_body, ["sessions"])
        if response.status == 201:
            ok = response.body.get("ok")
            if ok:
                self.token = response.body.get("token")
                return ok, response
            else:
                return ok, response
        else:
            return False, response

    def _create_user(self, body, content_type=MT_JSON):
        return self.post(body, ["users"], {}, content_type)

    def create_user(self, username, password):
        req_body = self.make_body(username=username, password=password)
        return self._create_user(req_body)


class Subscriptions(object):
    '''class to handle subscriptions state'''
    def __init__(self):
        self.subs = {}

    def format_key(self, bucket, stream):
        'format bucket and stream to a string key'
        return '%s:%s' % (bucket, stream)

    def format_subscription(self, parts):
        '''format a sub to raw from a 3 item array where the last one may be
        None'''
        bucket, stream, count = parts

        if count is None:
            return bucket + ':' + stream
        else:
            return bucket + ':' + stream + ':' + str(count)

    def add(self, bucket, stream, seqnum=None):
        '''add a subscription to the list'''
        key = self.format_key(bucket, stream)
        self.subs[key] = [bucket, stream, seqnum]

    def to_list(self):
        '''format to a list of strings to be used in a query'''
        return [self.format_subscription(sub) for sub in self.subs.values()]

    def update_seqnums(self, body):
        '''update to latest seqnum for each stream'''
        # done in two steps to go back to a smaller seqnum in case our
        # subscription is ahead of it
        latest = {}
        for event in body:
            meta = event['meta']
            bucket = meta['bucket']
            stream = meta['stream']
            seqnum = meta['id']

            key = bucket + ':' + stream

            if key in latest:
                current_count = latest[key][2]
                if current_count is None or seqnum > current_count:
                    latest[key][2] = seqnum
            else:
                latest[key] = [bucket, stream, seqnum]

        for key, val in latest.items():
            self.subs[key] = val

def show_response(resp, human=True):
    '''show content of request response'''
    code = resp.status_code
    has_body = bool(resp.text)
    content_type = resp.headers.get("Content-Type")
    try:
        json_body = json.loads(resp.text)
    except ValueError:
        json_body = None

    if human:
        print("Status:", code)
        print("Type:", content_type)

        if not has_body:
            print("No Response Body")
            return

        if json_body is None:
            print("Raw Response:", resp.text)
        else:
            print("JSON Response:")
            pprint.pprint(json_body)
    else:
        data = dict(code=code, has_body=has_body, content_type=content_type,
                body=json_body)
        print(json.dumps(data, indent=2))
