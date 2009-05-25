import struct
import t

@t.master()
def test_header(master):
    master.sock.send(struct.pack("!i", 2**30))
    t.raises(t.ServerError, master.read)

@t.master()
def test_invalid_json(master):
    master.write("foo")
    t.raises(t.ServerError, master.read)

@t.master()
def test_invalid_messge(master):
    master.send({"foo": "bar"})
    t.raises(t.ServerError, master.read)

@t.master()
def test_submit(master):
    id = master.submit(id=None, type="foo", body={"something": "here"})
    t.ne(id, None)

@t.master()
def test_join_error(master):
    master.send({"action": "join", "type": "foo"})
    t.raises(t.ServerError, master.read)

@t.master()
def test_respond_error(master):
    master.send({"action": "respond", "body": "bizzle"})
    t.raises(t.ServerError, master.read)


