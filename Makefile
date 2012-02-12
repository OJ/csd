.PHONY: deps

REBAR=`which rebar || ./rebar`

all: deps compile

compile:
	@$(REBAR) compile

app:
	@$(REBAR) compile skip_deps=true

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

test: app
	@$(REBAR) eunit skip_deps=true

webstart: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -config $(PWD)/apps/csd_web/priv/app.config -config $(PWD)/apps/csd_core/priv/app.config -s reloader -s csd_core -s csd_web

proxystart:
	@haproxy -f dev.haproxy.conf

