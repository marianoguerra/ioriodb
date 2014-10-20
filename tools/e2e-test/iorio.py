import json

def post_json(rsession, url, data, token=None):
    headers = {'content-type': 'application/json'}

    if token:
        headers['x-session'] = token

    return rsession.post(url, headers=headers, data=data)

def post_data_json(rsession, url, data, token=None):
    return post_json(rsession, url, json.dumps(data), token)

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
    if response.status_code == 200:
        return body.get("ok"), body.get("token")
    else:
        return False, None

def send(rsession, host, port, bucket, stream, data, token=None):
    url = format_url(host, port, "streams", bucket, stream)
    return post_data_json(rsession, url, data, token)

def get_json(rsession, url, token=None):
    headers = {'content-type': 'application/json'}

    if token:
        headers['x-session'] = token

    response = rsession.get(url, headers=headers)
    return response

def query(rsession, host, port, bucket, stream, limit, token=None):
    url = format_url(host, port, 'streams', bucket, stream, limit=limit)
    return get_json(rsession, url, token)


