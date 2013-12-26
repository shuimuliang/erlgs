REBAR=./rebar
APP_DIR=.

all: get-deps compile

get-deps:
	 @$(REBAR) get-deps
compile:
	 @$(REBAR) compile
generate: compile
	 @$(REBAR) generate
clean:
	 @$(REBAR) clean
rel: clean all
	 @$(REBAR) generate

debug:
	erl -pa ./ebin ./deps/*/ebin
tests: eunit ct
ct:
	@find ${APP_DIR}/src -type f -exec cp {} ${APP_DIR}/ebin \;
	@find ${APP_DIR}/test -type f -exec cp {} ${APP_DIR}/ebin \;
	${REBAR} skip_deps=true ct
	@find ${APP_DIR}/ebin -type f -name "*.erl" -exec rm {} \;
eunit:
	 @$(REBAR) skip_deps=true eunit
dialyze:
	dialyzer -I include/ src/*.erl test/*.erl

check:
	 @$(REBAR) generate
	 @$(REBAR) skip_deps=true xref
doc:
	 @$(REBAR) doc

update01: generate
	 mv rel/erlgs rel/erlgs01
update02: generate
	 @$(REBAR) generate-appups previous_release=erlgs01
	 @$(REBAR) generate-upgrade previous_release=erlgs01
	 mv rel/erlgs_2.tar.gz rel/erlgs01/releases/
