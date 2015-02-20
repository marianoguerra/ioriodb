'''iorio python api'''
try:
    import json
except ImportError:
    import simplejson as json

import time
import pprint
import urllib

try:
    import requests
except ImportError:
    import oldreq as requests

PERM_GET = "get"
PERM_STREAM_GET = "get"
PERM_STREAM_PUT = "put"
PERM_STREAM_GRANT = "grant"
PERM_BUCKET_GET = "get"
PERM_BUCKET_PUT = "put"
PERM_BUCKET_GRANT = "grant"
PERM_BUCKET_LIST = "list"

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

    def __str__(self):
        lines = ["Status: " + str(self.status),
                 "Type: " + self.content_type]


        if not self.raw_body:
            lines.append("No Response Body")
        elif self.body is None:
            lines.append("Raw Response: " + self.raw_body)
        else:
            lines.append("JSON Response: " + pprint.pformat(self.body))

        return "\n".join(lines)

    def to_json(self):
        return self.__dict__

class Connection(object):
    MT_JSON = 'application/json'
    MT_JSON_PATCH = 'application/json-patch+json'

    def __init__(self, host, port, secure=False, session_header_name='x-session'):
        self.host = host
        self.port = port
        self.session = requests.Session()
        self.secure = secure
        self.token = None
        self.username = None
        self.session_header_name = session_header_name

    def format_url(self, paths, query_params=None):
        if query_params:
            params = "?" + "&".join(("%s=%s" % (key, str(val))) for key, val in query_params.items())
        else:
            params = ""

        path = "/".join(str(item) for item in paths)

        if self.secure:
            protocol = 'https'
        else:
            protocol = 'http'

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

    def send(self, bucket, stream, data, content_type=MT_JSON):
        return self.post(json.dumps(data), ['streams', bucket, stream], None,
                content_type)

    def send_patch(self, bucket, stream, data, content_type=MT_JSON_PATCH):
        return self.patch(json.dumps(data), ['streams', bucket, stream],
                None, content_type)

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

    def query(self, bucket, stream, fromsn=None, limit=1):
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
                self.username = username
                self.token = response.body.get("token")
                return ok, response
            else:
                return ok, response
        else:
            return False, response

    def access_bucket(self, username, permission, bucket, action):
        req_body = self.make_body(username=username, permission=permission,
                                    action=action)
        return self.post(req_body, ["access", bucket])

    def access_stream(self, username, permission, bucket, stream, action):
        req_body = self.make_body(username=username, permission=permission,
                                    action=action)
        return self.post(req_body, ["access", bucket, stream])

    def grant_bucket(self, username, permission, bucket):
        return self.access_bucket(username, permission, bucket, 'grant')

    def revoke_bucket(self, username, permission, bucket):
        return self.access_bucket(username, permission, bucket, 'revoke')

    def grant_stream(self, username, permission, bucket, stream):
        return self.access_stream(username, permission, bucket, stream, 'grant')

    def revoke_stream(self, username, permission, bucket, stream):
        return self.access_stream(username, permission, bucket, stream, 'revoke')

    def raw_create_user(self, body, content_type=MT_JSON):
        return self.post(body, ["users"], {}, content_type)

    def create_user(self, username, password):
        req_body = self.make_body(username=username, password=password)
        return self.raw_create_user(req_body)


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
                latest[key] = [bucket, stream, seqnum + 1]

        for key, val in latest.items():
            self.subs[key] = val

