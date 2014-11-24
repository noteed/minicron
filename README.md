# Minicron

Minicron runs tasks.

## Design

There is a wakeup service, and then the tasks service.

The wakeup service simply sends scheduled wakeup messages. It maintains a
single pending wakeup, which can be modified or canceled.

The tasks service maintains the set of tasks to be run. When it receives a
wakeup message, it tries to run tasks. It schedules wakeups using the wakeup
service.

In the wakeup service, there are two threads. The main thread spawns the second
thread. The second thread sleeps the requested amount of time, then notifies
the main thread it is time to send a wakeup. The main thread also listens for
requests for a new wakeup value or for cancellation.
