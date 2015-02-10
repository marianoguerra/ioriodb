from __future__ import print_function
import sys
import time
import threading

import iorio

def make_requester(bucket, stream, conn, i, id):
    def requester():
        data = [{"op": "add", "path": "/items/-", "value": i}]
        retries = 0
        do_request = True
        reqs = []

        while do_request:
            result = conn.send_patch(bucket, stream, data)
            reqs.append([id, i, result.status])

            if result.status == 200:
                do_request = False
            else:
                retries += 1
                do_request = retries < 20

        #for req in reqs:
        #    print(req)

        return result

    return requester

def check_race_condition(bucket, stream, conn, count, id):
    result = conn.send(bucket, stream, {'items': []})
    #print(result)

    threads = [threading.Thread(
        target=make_requester(bucket, stream, conn, i, id)) for i in range(count)]

    for thread in threads:
        thread.daemon = True
        thread.start()

    for thread in threads:
        thread.join()

    result = conn.query(bucket, stream)
    items = result.body[0]['data']['items']

    if len(items) >= count:
        print('patch ok (%d) %s' % (len(items), items))
        return True
    else:
        print('patch error, expected {} items got {}'.format(count, len(items)))
        return False

def main():
    username = 'admin'
    password = 'secret'
    bucket = '_user_' + username
    stream = 'patch_race_condition_test'
    host = 'localhost'
    port = 8080

    conn = iorio.Connection(host, port)
    auth_ok, auth_resp = conn.authenticate(username, password)
    count = 0

    if auth_ok:
        ok = True
        while ok:
            count += 1
            ok = check_race_condition(bucket, stream, conn, 10, count)
    else:
        print('error authenticating', auth_resp)





if __name__ == '__main__':
    main()
