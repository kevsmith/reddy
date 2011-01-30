REDIS_HOST := '127.0.0.1'
REDIS_PORT := '6379'

all: compile test

compile:
	@./rebar compile

test:
	@./rebar eunit

eunit:
	@./rebar eunit

clean:
	@./rebar clean
	@rm -rf itest_ebin

itest_compile:
	@mkdir -p itest_ebin
	@cd itests;erlc -o ../itest_ebin *.erl

itest: compile itest_compile
	erl -noshell -boot start_sasl -pa itest_ebin -pa ebin -eval 'reddy_itests:run("${REDIS_HOST}", ${REDIS_PORT}).' -s erlang halt
	@rm -rf itest_ebin

help:
	@echo 'Available targets are:'
	@echo '     all:       compile & run unit tests \(recommended\)'
	@echo '     compile:   compile code only'
	@echo '     itest:     executes integration tests (requires a running redis server)'