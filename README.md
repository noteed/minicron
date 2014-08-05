# Minicron

Minicron runs tasks.

## Design

There are two, or more, threads. The main thread waits until either a task can
be run (and runs it), or the list of tasks is changed. The second thread is
spawn by the main thread. It sleeps a specified amount of time (given by the
mean thread), and then notify the main thread. This is how the main thread
knows it is time to run a task.

The other threads implement different mechanisms to change the list of tasks.
They can query a database, or listen on a socket. When the list of tasks is
changed, the main thread kill the sleep thread, and reschedule a new sleep
thread.

Actually this design is bad. Since the tasks can be maintained outside of this
program (e.g. in a database), there will be synchronisation issues (running a
task should modify it in the database too, not only in our in-memory
representation).
