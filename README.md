Gloom: Job Distribution Daemon
==============================

A simple Erlang job distribution daemon. This is completely non-durable and only exists to distribute the jobs to nodes that may reside on separate phsyical hosts.

Basic Protocol
--------------

All communication is done by sending a 32bit integer in network byte order followed by that number of bytes that represent a JSON payload.

Slaves
------

Connect a socket to your Gloom server on port 9999 and put a JSON message that looks like {"type": "join", "job_type": "echo"}. All jobs passing through the system have a type. A physical host can have as many clients serving as many different types as you want.

Then wait for a job message to come to you. Job messages look like: {"type": "job", "id": "message_id", "body": ...some json...}. After doing whatever, send back a response that looks like {"type": "response", "body": ...some json...}.

And then wait for another job.

To disconnect from the server, just close the socket. No special messaging required.

Each socket connection is only allowed to process a single type of job. If you want you can resend a {"type": "join", ...} message to recast the type of jobs the client wants to consume.

Master
------

Connect a socket to your Gloom server on port 9998 and send messages of the form: {"type": "job", "job_type": "echo", "id": "message_id", "body": ...some json...}

The message_id field must be unique within the system. You can resubmit a job with the same id once you get a response, but it will cause an error to submit it again before this time.

After being sent a slave will receive a message with the id and body and perform the necessary job tasks and then send the response back which is of the form {"id": "message_id", "body": ...json response...}.

No mas
------

That is everything. Its simple and I haven't tested it that much. Don't expect it to get many new features. The only thing I have plans to look into is adding a couple extra commands to the master side to get info on currently running jobs.
