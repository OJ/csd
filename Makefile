.PHONY: deps

REBAR=./rebar
RIAK_MOD_DIR=/tmp/riak

all: deps compile modules

modules:
	@cd apps/csd_core/riak_modules \
		&& erlc *.erl \
		&& mkdir -p ${RIAK_MOD_DIR} \
		&& cp ./*.beam ${RIAK_MOD_DIR}

compile: modules
	@$(REBAR) compile

app: modules
	@$(REBAR) compile skip_deps=true

depmod:
	~/code/riak/dev/dev1/bin/riak-admin erl-reload \
		&& ~/code/riak/dev/dev2/bin/riak-admin erl-reload \
		&& ~/code/riak/dev/dev3/bin/riak-admin erl-reload \
		&& ~/code/riak/dev/dev4/bin/riak-admin erl-reload

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean && rm apps/csd_core/riak_modules/*.beam

distclean: clean
	@$(REBAR) delete-deps

test: app
	@$(REBAR) eunit skip_deps=true

webstart: app
	@exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl \
		-config $(PWD)/apps/csd_web/priv/app.config \
		-config $(PWD)/apps/csd_core/priv/app.config \
		-s reloader \
		-s csd_core \
		-s csd_web

proxystart:
	@haproxy -f $(PWD)/priv/dev.haproxy.conf
