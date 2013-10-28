ERL ?= erl
APP := dirbusterl
RFC1808=priv/rfc1808.txt

.PHONY: deps test

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

test: $(RFC1808) all
	@./rebar eunit

$(RFC1808):
	wget http://tools.ietf.org/rfc/rfc1808.txt -O $@

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
