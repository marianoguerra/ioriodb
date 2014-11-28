'''iorio python api'''
from __future__ import print_function

import json
import pprint
import urllib
import requests

def body_json(rsession, url, data, method, token=None, content_type='application/json'):
    headers = {'content-type': content_type}

    if token:
        headers['x-session'] = token

    return getattr(rsession, method)(url, headers=headers, data=data)

def body_data_json(rsession, url, data, method, token=None, content_type='application/json'):
    return body_json(rsession, url, json.dumps(data), method, token, content_type)

def post_data_json(rsession, url, data, token=None, content_type='application/json'):
    return body_data_json(rsession, url, data, 'post', token, content_type)

def patch_data_json(rsession, url, data, token=None, content_type='application/json-patch+json'):
    return body_data_json(rsession, url, data, 'patch', token, content_type)

def format_url(host, port, *paths, **query_params):
    if query_params:
        params = "?" + "&".join(("%s=%s" % (key, str(val))) for key, val in query_params.items())
    else:
        params = ""

    path = "/".join(str(item) for item in paths)
    return 'http://%s:%d/%s%s' % (host, port, path, params)

def authenticate(rsession, host, port, username, password):
    url = format_url(host, port, "sessions")
    req_body = dict(username=username, password=password)
    response = post_data_json(rsession, url, req_body)
    body = json.loads(response.text)
    if response.status_code == 201:
        ok = body.get("ok")
        if ok:
            return ok, body.get("token")
        else:
            return ok, response
    else:
        return False, response

def send(rsession, host, port, bucket, stream, data, token=None,
        content_type='application/json'):
    url = format_url(host, port, "streams", bucket, stream)
    return post_data_json(rsession, url, data, token, content_type)

def patch(rsession, host, port, bucket, stream, data, token=None,
          content_type='application/json-patch+json'):
    url = format_url(host, port, "streams", bucket, stream)
    return patch_data_json(rsession, url, data, token, content_type)

def list_buckets(rsession, host, port, token=None):
    url = format_url(host, port, "buckets")
    return get_json(rsession, url, token)

def list_streams(rsession, host, port, bucket, token=None):
    url = format_url(host, port, "streams", bucket)
    return get_json(rsession, url, token)

def listen(rsession, host, port, subs, token=None):
    url_base = format_url(host, port, "listen", jwt=urllib.parse.quote(token))
    url = url_base + '&' + '&'.join(['s=%s' % sub for sub in subs])
    return get_json(rsession, url, token)

def get_json(rsession, url, token=None):
    headers = {'content-type': 'application/json'}

    if token:
        headers['x-session'] = token

    response = rsession.get(url, headers=headers)
    return response

def query(rsession, host, port, bucket, stream, fromsn, limit, token=None):
    params = {'limit': limit}
    if fromsn is not None:
        params['from'] = fromsn

    url = format_url(host, port, 'streams', bucket, stream, **params)
    return get_json(rsession, url, token)

def new_session():
    '''return a new http session'''
    return requests.Session()

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
