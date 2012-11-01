ERL ?= erl
APP := veda

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

app:
	@./rebar compile skip_deps=true

webstart: app
	@exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s veda@client -s veda@core

proxystart:
	@sudo haproxy -f haproxy.cfg
