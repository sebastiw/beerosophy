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
Starts `beerosophy_server.erl`, `beerosophy_metrics.erl`,
`beerosophy_python_sup.erl`, and `beerosophy_ticker_sup.erl`


beerosophy_server.erl
=====

Starts the webserver, Cowboy.
Reads the `pylons.conf` and starts them.

beerosophy_ticker.erl
=====

One `ticker` is started for each metric.

At a tick (representation of a given interval) the measurements
are gathered and then stored for a specific sensor metric.


beerosophy_metrics.erl
=====

Cowboy rest API for getting to know some metrics like number of errors, etc.


beerosophy_sensors.erl
=====

NOT INVENTED YET. Might not even be needed.

Cowboy rest API for attaching and handling sensors.
Communicates with `beerosophy_python_sup.erl` and `beerosophy_python.erl`.


beerosophy_python_sup.erl
=====

Supervises `beerosophy_python.erl`.


beerosophy_python.erl
=====

Start and supervise Python scripts. One process for each script.


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

You will need [rebar3](github.com/otp/rebar3).

    $ rebar3 compile
