.PHONY: all doc readme compile run run_debug dialyzer typer clean clean_all

all: compile

doc:
	rebar skip_deps=true doc

readme:
	pandoc README.md -o README.html

deps/yaws:
	rebar get-deps

compile: deps/yaws
	rebar compile

run: compile
	erl -pa deps/goldrush/ebin -pa deps/ibrowse/ebin -pa deps/lager/ebin -pa deps/yaws/ebin -pa ebin -eval 'application:start (visual_nn).'

run_debug: compile
	erl -pa deps/goldrush/ebin -pa deps/ibrowse/ebin -pa deps/lager/ebin -pa deps/yaws/ebin -pa ebin -eval \
		'application:start (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

dialyzer:
	dialyzer --fullpath -Wrace_conditions -r ebin

typer:
	typer -pa deps/lager -r src

clean:
	rebar skip_deps=true clean

clean_all:
	rebar skip_deps=true clean
	rm -f README.html
	rm -f report.log
	rm -rf ebin/
	rm -rf doc/
	rm -rf log/
