import struct
import t

@t.slave()
def test_header(slave):
    slave.sock.send(struct.pack("!i", 2**30))
    t.raises(t.ServerError, slave.read)

@t.slave()
def test_invalid_json(slave):
    slave.send(''.join([struct.pack("!i", 3), "foo"]))
    t.raises(t.ServerError, slave.read)

@t.slave()
def test_invalid_messge(slave):
    slave.write({"foo": "bar"})
    t.raises(t.ServerError, slave.read)

@t.slave()
def test_join(slave):
    slave.join(type="foo")

@t.slave()
def test_rejoin(slave):
    slave.join(type="foo")
    slave.join(type="bar")

@t.slave()
def test_respond(slave):
    t.raises(t.ServerError, slave.respond, body={"some": "stuff"})

@t.slave()
def test_submit_error(slave):
    slave.write({"action": "submit", "id": "mesage"})
    t.raises(t.ServerError, slave.read)
