thicc
=====
The Trivial HTTP Interface for (Docker) Compose Containers - or *thicc* -
is a daemon which sits around and waits for callbacks from Docker Hub
(or any other Docker repository which supports hooks).

When such a callback arrives, its corresponding docker image is updated to the
latest version and restarted.

thicc is written first and foremost for simplicity and security.
It is intended for smaller deployments, probably running on a single machine.
If you try to use it to run your Facebook or Google, you will be severely
disappointed.


Installing
----------
Assuming you're running a Debian-based (Debian, Ubuntu, Mint, etc.) Linux
distribution:

1. Add `deb https://deb.ekblad.cc/ubuntu disco main` to your
    `/etc/apt/sources.list`,
2. add my [signing key](https://ekblad.cc/key.asc) to your system by running
    `wget -O - https://ekblad.cc/key.asc | sudo apt-key add -`, and
3. run `sudo apt update && sudo apt install thicc` to install thicc.


Usage
-----
Thicc acts as a sort of manager for your docker-compose files.
Any applications managed by thicc will be started or stopped when thicc starts
or stops respectively, and will be updated (using `docker-compose pull`)
whenever their respective webhook is called.

To start using thicc, you first need to add your application to it.
An application is represented by a *name* and a *docker-compose.yaml* file.
It is recommended that the image referred to by your compose file is hosted
on Docker Hub or similar. If it's in a private repository, you also need to
authenticate with the host beforehand.

To add an application:

```shell
sudo thicc add my-app my-app/docker-compose.yaml
```

This will add the application described by `my-app/docker-compose.yaml`
to thicc under the name `my-app`.

Then, you need to enable your newly added app:

```shell
sudo thicc enable my-app
```

This will start your application and keep it running according to its
restart policy as specified in its compose file.

To add update-on-webhook functionality to your application, which is probably
what you're here for, you need to obtain the webhook URL for your app.
This can be accomplished using the `thicc url` command:

```shell
sudo thicc url my-app example.com
```

The first argument to `url` is the name of your application, and the second is
the the public host name or address at which the machine you're running thicc on
can be reached.
This will print an URL looking something like
`http://example.com:12321/c/my-app/a-long-string-of-gibberish`.

Now, go to your Docker repository, add a new webhook, give it an appropriate
name, paste that URL into the webhook URL field, and hit "create".
Congratulations, now your application will be automatically updated whenever
a new version of its image is pushed to your repository!

For more information about what else thicc can do, try running `thicc help`.


Q&A
===
* Does thicc support webhooks over HTTPS?
    - Not yet. Soon.
* Does thicc support zero-downtime upgrades.
    - Not yet. When a webhook arrives, the following steps are executed:
        
        1. The new image is pulled.
        2. The old container is stopped.
        3. A new container is started using the new image.
        4. (If configured) `docker image prune` is used to clean up old images.
        
        Support for starting the new container before taking down the old one,
        seamlessly switching any exposed ports over to the new container,
        but there is no estimate for when this will actually be implemented.
