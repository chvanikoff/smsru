REBAR = `which rebar || echo ./rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

debug:
	@( erl -pa ebin deps/*/ebin -s smsru )

.PHONY: all deps compile debug
