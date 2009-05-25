#! /usr/bin/env python

import socket
import struct
import uuid

try:
    import json
except ImportError:
    import simplejson as json

class GloomError(Exception):
    pass

class TransportError(GloomError):
    pass

class ProtocolError(GloomError):
    pass

class ServerError(GloomError):
    def __init__(self, data):
        self.error = data.get("error", "unknown error")
        self.reason = data.get("reason", "unknown cause")
    
    def __str__(self):
        return "%s -> %s" % (self.error, self.reason)

class Job(dict):
    id = property(fget=lambda x: x.get("id"), doc="Job id.")
    type = property(fget=lambda x: x.get("type"), doc="Job type.")
    body = property(fget=lambda x: x.get("body"), doc="Job body.")

class Connection(object):
    def __init__(self, ip, port):
        self.sock = socket.socket()
        self.sock.connect((ip, port))

    def disconnect(self):
        self.sock.close()

    def write(self, data):
        packet = ''.join([struct.pack("!i", len(data)), data])
        self.sock.sendall(packet)

    def send(self, data):
        self.write(json.dumps(data))

    def read(self, timeout=None):
        self.sock.settimeout(timeout)
        length = self.sock.recv(4)
        if len(length) < 4:
            raise TransportError("Failed to read length.")
        (length,) = struct.unpack("!i", length)
        data = self.sock.recv(length)
        if len(data) < length:
            raise TransportError("Failed to read JSON payload.")
        ret = json.loads(data)
        if "error" in ret:
            raise ServerError(ret)
        return ret

class Slave(Connection):
    def __init__(self, ip="127.0.0.1", port=9999):
        super(Slave, self).__init__(ip, port)

    def __iter__(self):
        return self

    def next(self):
        while True:
            yield self.job()

    def join(self, type):
        self.send({"action": "join", "type": type})
        ret = self.read()
        if "ok" not in ret or ret["ok"] != True:
            raise ProtocolError("Unexpected response: %r" % ret)
        return True

    def job(self, timeout=None):
        ret = self.read(timeout=timeout)
        for k in ["action", "id", "type", "body"]:
            if k not in ret:
                raise ProtocolError("Expected '%s' in job description." % k)
        return Job(ret)

    def respond(self, body=None):
        self.send({"action": "respond", "body": body})
        ret = self.read()
        if "ok" not in ret or ret ["ok"] != True:
            raise ProtocolError("Unexpected response: %r" % ret)
        return True

class Master(Connection):
    def __init__(self, ip="127.0.0.1", port=9998):
        super(Master, self).__init__(ip, port)

    def new_job_id(self):
        return uuid.uuid4().hex.upper()

    def submit(self, id=None, type=None, body=None):
        if id is None: id = self.new_job_id()
        self.send({"action": "submit", "id": id, "type": type, "body": body})
        ret = self.read()
        if "ok" not in ret or ret["ok"] != True:
            raise ProtocolError("Unexpected response: %r" % ret)
        return id

    def receive(self, timeout=None):
        ret = self.read(timeout=timeout)
        for k in ["id", "body"]:
            if k not in ret:
                raise ProtocolError("Expected '%s' in job response." % k)
        return Job(ret)

    def do_many(self, jobs, timeout=None):
        ids = set([self.submit(**j) for j in jobs])
        while len(ids):
            job = self.receive(timeout=timeout)
            ids.remove(job.id)
            yield job
