Task Manager
============

This system manages a set of processes - specifically, you can

   1. start
   2. receive output from
   3. request the status of
   4. receieve the status of
   5. request the termination of
   6. be notified of the termination of

a process.  In addition, you can get request and receive a list of
running processes from the manager, and ask the manager to exit as
soon as all processes finish.

It is implemented using haskell threads and MVars.  The manager of all
the tasks runs in a thread, and each task has a thread which manages
communication between the manager and the process.  The message types
are named after their origin and destination.  Here we call the client
that is using the task manager "Top":

	+-----------------------+
	|         Top           |  type TopTakes = ManagerToTop
	+-----------------------+
	     |            ^
	     |            |
	TopToManager ManagerToTop
	     |            |
	     v            |
	+-----------------------+
	|       Manager         |  data ManagerTakes = TopToManager | TaskToManager
	+-----------------------+
	     |              ^
	     |              |
	ManagerToTask TaskToManager
	     |              |
	     v              |
	+-----------------------+
	|        Task N         |  data TaskTakes = ManagerToTask | ProcessToTask
	+-----------------------+
	     |              ^
	     |              |
	TaskToProcess ProcessToTask
	     |              |
	     v              |
	+-----------------------+
	|       Process N       |  type ProcessTakes = TaskToProcess
	+-----------------------+
