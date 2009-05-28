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

class Protocol(object):
    def __init__(self, sock):
        self.sock = sock

    def disconnect(self):
        self.sock.close()

    def send(self, data, timeout=None):
        self.sock.settimeout(timeout)
        packet = ''.join([struct.pack("!i", len(data)), data])
        self.sock.sendall(packet)

    def recv(self, timeout=None):
        self.sock.settimeout(timeout)
        length = self.sock.recv(4)
        if len(length) < 4:
            raise TransportError("Failed to read length.")
        (length,) = struct.unpack("!i", length)
        data = self.sock.recv(length)
        if len(data) < length:
            raise TransportError("Failed to read payload.")
        return data

class Connection(Protocol):
    def __init__(self, addr):
        if isinstance(addr, tuple):
            sock = socket.socket()
            sock.connect(addr)
            super(Connection, self).__init__(sock)
        elif isinstance(addr, socket.socket):
            super(Connection, self).__init__(addr)
        elif isinstance(addr, Protocol):
            super(Connection, self).__init__(addr.sock)
        else:
            mesg = "'%s' is not a tuple, socket, or gloom.Protocol object."
            raise TypeError(mesg % addr.__class__.__name__)

    def write(self, data, timeout=None):
        self.send(json.dumps(data), timeout=timeout)

    def read(self, timeout=None):
        ret = json.loads(self.recv(timeout=timeout))
        if "error" in ret:
            raise ServerError(ret)
        return ret

class Master(Connection):
    def __init__(self, addr=("127.0.0.1", 9998)):
        super(Master, self).__init__(addr)

    def new_job_id(self):
        return uuid.uuid4().hex.upper()

    def submit(self, id=None, type=None, body=None, timeout=None):
        if id is None: id = self.new_job_id()
        data = {"action": "submit", "id": id, "type": type, "body": body}
        self.write(data, timeout=timeout)
        ret = self.read(timeout=timeout)
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

class Slave(Connection):
    def __init__(self, addr=("127.0.0.1", 9999)):
        super(Slave, self).__init__(addr)

    def __iter__(self):
        return self

    def next(self):
        while True:
            yield self.job()

    def join(self, type, timeout=None):
        self.write({"action": "join", "type": type}, timeout=timeout)
        ret = self.read(timeout=timeout)
        if "ok" not in ret or ret["ok"] != True:
            raise ProtocolError("Unexpected response: %r" % ret)
        return True

    def job(self, timeout=None):
        ret = self.read(timeout=timeout)
        for k in ["action", "id", "type", "body"]:
            if k not in ret:
                raise ProtocolError("Expected '%s' in job description." % k)
        return Job(ret)

    def respond(self, body=None, timeout=None):
        self.write({"action": "respond", "body": body}, timeout=timeout)
        ret = self.read(timeout=timeout)
        if "ok" not in ret or ret ["ok"] != True:
            raise ProtocolError("Unexpected response: %r" % ret)
        return True
