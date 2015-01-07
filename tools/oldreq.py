'''a thin shim for the parts of requests we use'''
import urllib2

class BetterHTTPErrorProcessor(urllib2.BaseHandler):
    # a substitute/supplement to urllib2.HTTPErrorProcessor
    # that doesn't raise exceptions on status codes 201,204,206
    def http_error_201(self, request, response, code, msg, hdrs):
        return response
    def http_error_204(self, request, response, code, msg, hdrs):
        return response
    def http_error_206(self, request, response, code, msg, hdrs):
        return response
    def http_error_400(self, request, response, code, msg, hdrs):
        return response
    def http_error_401(self, request, response, code, msg, hdrs):
        return response
    def http_error_415(self, request, response, code, msg, hdrs):
        return response
    def http_error_500(self, request, response, code, msg, hdrs):
        return response

opener = urllib2.build_opener(BetterHTTPErrorProcessor)
urllib2.install_opener(opener)

class Response(object):
    def __init__(self, status_code, text, headers):
        self.status_code = status_code
        self.text = text
        self.headers = headers

    @classmethod
    def from_urllib2_response(cls, response):
        info = response.info()
        data = response.read()
        code = response.code

        return cls(code, data, info)

    def __str__(self):
        return '<oldreq.Response status=%d>' % self.status_code

class Session(object):
    def __init__(self):
        pass

    def query_req(self, method, url, headers=None):
        if headers is None:
            headers = {}

        request = urllib2.Request(url, headers=headers)
        request.get_method = lambda: method
        response = urllib2.urlopen(request)

        return Response.from_urllib2_response(response)

    def body_req(self, method, url, data, headers=None):
        if headers is None:
            headers = {}

        request = urllib2.Request(url, data, headers)
        request.get_method = lambda: method
        response = urllib2.urlopen(request)

        return Response.from_urllib2_response(response)

    def post(self, url, data, headers=None):
        return self.body_req('POST', url, data, headers)

    def put(self, url, data, headers=None):
        return self.body_req('PUT', url, data, headers)

    def patch(self, url, data, headers=None):
        return self.body_req('PATCH', url, data, headers)

    def get(self, url, headers=None):
        return self.query_req('GET', url, headers)

    def delete(self, url, headers=None):
        return self.query_req('DELETE', url, headers)
