Beerosophy
=====

A library for getting metrics from different sensors when brewing beers.

This will hopefully run on a Raspberry Pi.


Internals
-----

beerosophy.erl
=====

Main API, starts the application `beerosophy_app.erl`


beerosophy_app.erl
=====

Application.
Starts the applications main supervisor `beerosophy_sup.erl`


beerosophy_sup.erl
=====

Main supervisor.
Start childs `beerosophy_server.erl` and `beerosophy_database.erl`


beerosophy_server.erl
=====

Starts the webserver, Cowboy.


beerosophy_database.erl
=====

Cowboy rest API for storing stuff in database.


beerosophy_metrics.erl
=====

Cowboy rest API for getting to know some metrics like number of errors, etc.


beerosohpy_sensors.erl
=====

Cowboy rest API for attaching and handling sensors.
Communicates with `beerosophy_python_sup.erl` and `beerosophy_python.erl`.


beerosophy_python_sup.erl
=====

Supervises `beerosophy_python.erl`.


beerosophy_python.erl
=====

Uses [ErlPort](https://github.com/hdima/erlport) to start and supervise python
scripts.


Priv/
-----

dispatch.conf
=====

Holds Cowboy routes.


pylons.conf
=====

Holds Python scripts that should be automatically supervised.


Build
-----

    $ rebar3 compile
