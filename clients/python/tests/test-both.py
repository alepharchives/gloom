import t

@t.both()
def test_single_job(master, slave):
    master.submit(id="hi", type="foo", body="Here")
    slave.join("foo")
    job = slave.job(timeout=1)
    t.eq(job, {"action": "job", "id": "hi", "type": "foo", "body": "Here"})
    slave.respond(body="ohai!")
    t.eq(master.receive(timeout=1), {"id": "hi", "body": "ohai!"})

