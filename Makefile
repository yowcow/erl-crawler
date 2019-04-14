REBAR := rebar3

all:
	$(REBAR) compile

shell:
	$(REBAR) shell

test:
	$(REBAR) eunit

release: _build/default/rel/crawler/crawler-0.1.0.tar.gz
	mv $< .

_build/%/rel/crawler/crawler-0.1.0.tar.gz:
	$(REBAR) as $* tar

clean:
	-rm -rf _build *.tar.gz

.PHONY: all shell run test clean
