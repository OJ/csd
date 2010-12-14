ERL ?= erl
APP := csd
PWD := $(shell pwd)

.PHONY: deps
.PHONY: proxy-start
.PHONY: start

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

proxy-start:
	sudo haproxy -f dev.haproxy.conf -d

start:
	exec erl -pa $(PWD)/apps/*/ebin $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s csd_core -s csd_web
