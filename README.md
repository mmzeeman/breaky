Fusebox (work in progress)
==========================

Fusebox provides a way to supervise and manage modules and processes depending on external resources.

For these kind of processes supervisors will not work. The problem is that if something fails a 
supervisor normally restarts the client part too fast which can lead to a failed supervisor, which
can lead to a cascading failures in your system. 

Fusebox acts as a "middleman" for these kind of processes. They are started and managed by an Erlang
fuse FSM. When it detects a failure or slow responses it manages restarts and access to the process.


