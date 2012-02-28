REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
.PHONY: all edoc test clean

all:
	@$(REBAR) get-deps compile

test:
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(REBAR) skip_deps=true doc

clean:
	@$(REBAR) clean
