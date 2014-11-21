import json
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
          content_type='application/json'):
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

def query(rsession, host, port, bucket, stream, limit, token=None):
    url = format_url(host, port, 'streams', bucket, stream, limit=limit)
    return get_json(rsession, url, token)

def new_session():
    '''return a new http session'''
    return requests.Session()
