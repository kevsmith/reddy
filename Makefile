all: compile test

compile:
	./rebar compile

test:
	./rebar eunit

eunit:
	./rebar eunit

clean:
	./rebar clean