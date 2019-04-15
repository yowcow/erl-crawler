erl-crawler
===========

Parallel HTTP client, RSS parser, and database writer.

HOW TO ...
----------

    $ make clean all test release shell

HOW TO START/STOP mysql
-----------------------

    $ make -f docker.mk start
    $ make -f docker.mk stop
    $ make -f docker.mk clean
