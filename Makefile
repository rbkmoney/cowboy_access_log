REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)

all: compile

compile:
	$(REBAR) compile

rebar-update:
	$(REBAR) update

test:
	$(REBAR) eunit

xref:
	$(REBAR) as test xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build

dialyze:
	$(REBAR) as test dialyzer

lint:
	elvis rock
