splicer
=====

A library to splice 2 sockets together, optionally using `splice(2)` when available.

Only one function is exported: `splicer:splice/2` it takes 2 file descriptors (use inet:getfd).
The function will block until the splice is finished (ie. one side of the splice is closed).

Build
-----

    $ rebar3 compile
