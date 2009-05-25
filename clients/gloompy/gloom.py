#! /usr/bin/env python

import simplejson
import socket
import struct
import sys
import time

port = int(sys.argv[1])

def connect(ip, port):
    s = socket.socket()
    s.connect((ip, port))
    return s

def send(sock, data):
    data = simplejson.dumps(data)
    hdr = struct.pack("!i", len(data))
    sock.send(''.join([hdr, data]))

def read(sock):
    hdr = sock.recv(4)
    if len(hdr) < 4:
        raise ValueError("Failed to receive length header.")
    (length,) = struct.unpack("!i", hdr)
    data = ""
    while len(data) < length:
        data += sock.recv(length-len(data))
    return simplejson.loads(data)

if int(sys.argv[1]) == 9999:
    sock = connect("127.0.0.1", int(sys.argv[1]))
    send(sock, {"type": "join", "job_type": "blast"})
    while True:
        job = read(sock)
        print job
        if "error" not in job:
            send(sock, {
                "type": "response",
                "id": job["id"],
                "body": {"foo": job}
            })
else:
    sock = connect("127.0.0.1", int(sys.argv[1]))
    send(sock, {"id": "z", "type": "job", "job_type": "blast", "body": "bamboo!"})
    job = read(sock)
    print job
