REBAR=./rebar
REBAR_COMPILE=$(REBAR) get-deps compile
RFC1808=priv/rfc1808.txt

compile:
	$(REBAR_COMPILE)

test: $(RFC1808)
	$(REBAR_COMPILE) eunit

$(RFC1808):
	wget http://tools.ietf.org/rfc/rfc1808.txt -O $@

clean:
	-rm -rf deps ebin .eunit

.PHONY: compile clean test
