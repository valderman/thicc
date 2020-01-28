thicc
=====

The Trivial HTTP Interface for (Docker) Compose Containers - or *thicc* -
is a daemon which sits around and waits for callbacks from Docker Hub
(or any other Docker repository which supports hooks).

When such a callback arrives, its corresponding docker image is updated to the
latest version and restarted.

thicc is written first and foremost for simplicity and security.
It is intended for smaller deployments, preferably running on a single machine.
If you try to use it to run your Facebook or Google, you will be severely
disappointed.