# sherl: Make Erlang talk to subprocesses like a boss

Erlang's built-in subprocess management facilities are good enough to run a
well behaved port, but aren't going to do you much good for running arbitrary
processes.

## Project goals

The goal for sherl is to be more or less on par with the capabilities of the
subprocess module for Python. This includes:

* Know the pid of the subprocess
* Get the exit status of the subprocess
* Flow control
* Take a list of arguments and escape them as appropriate, or just run
  a command with the default shell
* Route input via pipes or just inherit stdin/stdout/stderr
* Send signals to subprocess
* Poll or wait for exit of subprocess

The following are non-goals:

* Windows support
