
default: compile

.PHONY: test clean compile ct eunit xref dialyzer
.PHONY: release

test: xref eunit ct

rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod +x ./rebar3

clean: | rebar3
	./rebar3 clean

compile: | rebar3
	./rebar3 compile

ct: | rebar3
	./rebar3 ct --cover
	./rebar3 cover

eunit: | rebar3
	./rebar3 eunit

xref: | rebar3
	./rebar3 xref

dialyzer: | rebar3
	./rebar3 dialyzer

release: clean | rebar3
	./rebar3 xref
	./rebar3 eunit
	./rebar3 release

