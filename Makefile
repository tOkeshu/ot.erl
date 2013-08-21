all: tests

ebin:
	mkdir ebin

compile: ebin
	@cd ebin && erlc ../src/ot.erl ../test/ot_tests.erl

tests: compile
	@erl -noshell -pa ebin -eval "eunit:test(ot_tests, [verbose])" -s init stop

