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
	erl -noshell -boot start_sasl -pa itest_ebin -pa ebin -eval 'reddy_itests:run().' -s erlang halt
	@rm -rf itest_ebin

help:
	@echo 'Available targets are:'
	@echo '     all:       compile & run unit tests (RECOMMENDED)'
	@echo '     compile:   compile code only'
	@echo '     itest:     executes integration tests (requires a running redis server)'
	@echo '                Use env vars $$REDIS_HOST and $$REDIS_PORT to use a specific'
	@echo '                server for integration tests.'