REBAR := rebar3
BIN := _build/default/bin/erl_http

all:
	$(REBAR) compile

run: build
	./$(BIN)

build: $(BIN)

$(BIN): src/*.erl
	$(REBAR) escriptize

shell:
	$(REBAR) shell

test:
	$(REBAR) eunit

clean:
	-rm -rf _build

.PHONY: all shell run test clean
