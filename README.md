# Minicron

Minicron runs tasks.

## Design

There is a wakeup service, and then the tasks service.

The wakeup service simply sends scheduled wakeup messages. It maintains a
single pending wakeup, which can be replaced by another scheduled wakeup.

The tasks service maintains the set of tasks to be run. When it receives a
wakeup message, it tries to run tasks. It schedules wakeups using the wakeup
service. It can do so either by returning a value after running a task (when it
already knows another task will need a wakeup), or by requesting explicitely a
new wakeup (e.g. when it is notified by another service that a new task has
been added).

In the wakeup service, there are two threads. The main thread spawns the second
thread. The second thread sleeps the requested amount of time, then notifies
the main thread it is time to send a wakeup. The main thread also listens for
requests for a new wakeup value.

## Usage

The simple usage is to write a small Haskell script that creates a set of
tasks, then call `System.Cron.Mini.cron`. This will result in a program that
executes the tasks, until the set of tasks is empty (which might never be the
case if e.g. a task repeats endlessy).

The second usage is to write a client thread that will implement the tasks
service and communicate with the wakeup service. This makes it possible to
dynamically change the set of tasks and continue to run even if it becomes
empty. For instance the set of tasks can be recorded in a database, and the
client thread will wait for a record to appear before requesting a wakeup.
