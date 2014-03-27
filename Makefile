DEPS := $(wildcard deps/*/ebin)
TEST_ERL_AFLAGS := "-pa ebin $(DEPS)"

default: test

compile:
	rebar compile skip_deps=true

clean:
	rebar clean

test:
ifdef suites
	ERL_AFLAGS=$(TEST_ERL_AFLAGS) rebar eunit skip_deps=true suites=$(suites)
else
	ERL_AFLAGS=$(TEST_ERL_AFLAGS) rebar eunit skip_deps=true
endif

.PHONY: default compile clean test
