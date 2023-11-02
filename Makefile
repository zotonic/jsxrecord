REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
ERL       ?= erl

.PHONY: compile test

all: compile

compile: $(REBAR)
	$(REBAR) compile

shell: $(REBAR)
	$(REBAR) shell

test: $(REBAR)
	$(REBAR) ct --config rebar.test.config

dialyzer: $(REBAR)
	$(REBAR) as test dialyzer

xref: $(REBAR)
	$(REBAR) as test xref

clean: $(REBAR)
	$(REBAR) clean

edoc: $(REBAR)
	$(REBAR) edoc

./rebar3:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar3"}])' \
	  -s init stop
	chmod +x ./rebar3

bench.encode:
	cd jsxrecord_bench && mix encode

bench.decode:
	cd jsxrecord_bench && mix decode
