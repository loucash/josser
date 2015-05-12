PROJECT := josser

ERL := erl
EPATH = -pa ebin -pz deps/*/ebin
TEST_EPATH = -pa .eunit -pz deps/*/ebin
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS= -Wno_undefined_callbacks --fullpath

.PHONY: all build_plt compile configure console deps doc clean depclean distclean dialyze release telstart test test-console

all: deps compile

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile:
	@./rebar compile

configure:
	@./rebar get-deps compile

console:
	$(ERL) -sname $(PROJECT) $(EPATH)

deps:
	@./rebar get-deps update-deps

doc:
	@./rebar skip_deps=true doc

clean:
	@./rebar skip_deps=true clean

depclean:
	@./rebar clean

distclean:
	@./rebar clean delete-deps
	@rm -rf logs

dialyze:
	@dialyzer $(DIALYZER_OPTS) -r ebin

dialyzer-travis:
	kerl list installations
	@dialyzer --plt plts/travis-erlang-$(TRAVIS_OTP_RELEASE).plt $(DIALYZER_OPTS) -r ebin

start:
	$(ERL) -sname $(PROJECT) $(EPATH) -s $(PROJECT)

test:
	@./rebar skip_deps=true ct verbose=1

test-console: test
	$(ERL) -sname $(PROJECT)_test $(TEST_EPATH)
