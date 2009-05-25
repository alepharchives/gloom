Gloom: Job Distribution Daemon
==============================

A simple Erlang job distribution daemon. This is completely non-durable and only exists to distribute the jobs to nodes that may reside on separate physical hosts.

Basic Protocol
--------------

All communication is done by sending a 32 bit integer in network byte order followed by that number of bytes that represent a JSON payload.

Note on "body"
--------------

In all cases "body" can be any arbitrary JSON data.

Slaves
------

All slave interaction is done via port 9999 on your Gloom server.

Joining the queue:

    {"action": "join", "type": "echo"}
	
Response:

	{"ok": true}

Any host can connect as many clients as they wish. A single socket can only serve one type of job and only processes a single job at a time. If you want to process multiple jobs in parallel either run more processes or open multiple sockets.

If for some reason you want to switch a socket's job type you can just resend a new join message.

Job messages:

    {"action": "job", "id": "message_id", "body": ...}

Returning a response:

    {"action": "respond", "body": ...}

Response:

	{"ok": "true"}

To disconnect from the server, just close the socket. No special messaging required.

Master
------

Master connections are done on port 9998

Submitting jobs:

	{"action": "submit", "id": "message_id", "body": ... }
	{"action": "submit", "id": "message_id", "priority": 5, "body": ...}

Response:

	{"ok": true}

The message_id field must be unique within the system. You can resubmit a job with the same id once you get a response, but it will cause an error to submit it again before this time. It is recommended that you use hashes or UUIDs as job identifiers.

The "priority" member must be an integer. Larger values indicate higher priority. The default priority is 0. In the case that two jobs have an equal priority they are ordered by the time of submission with older submissions taking precedence.

Job response:

    {"id": "message_id", "body": ... json ...}

You are free to submit as many jobs as you want to the server. The order in which they are returned is not guaranteed to be the same order in which they were sent.

Disconnection:

You are free to disconnect a master at any point. Any jobs that this master submitted that are running will complete and have their response discarded. Any jobs that are still queued will be cleared from the queue.

Python client
-------------

There's a reference python client in 'clients/python/' that can serve as a reference for anyone looking to write their own. Clients for other languages are welcome.
