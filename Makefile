.PHONY: all doc readme compile run run_debug test dialyzer typer clean clean_all

DEPS=goldrush lager ibrowse yaws
PLT_APPS=erts kernel stdlib $(DEPS)

PA_DEPS_EBIN=$(DEPS:%=-pa deps/%/ebin)
EBINS=$(PA_DEPS_EBIN) -pa ebin

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
	erl $(EBINS) -eval 'application:start (visual_nn).'

run_debug: compile
	erl $(EBINS) -eval 'application:start (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

test:
	rebar skip_deps=true eunit $(TEST_CASE)

deps_plt:
	rm -f deps/yaws/ebin/mime_type_c.beam
	rm -f deps/yaws/ebin/yaws_generated.beam
	# --no_check_plt
	dialyzer $(PA_DEPS_EBIN) --output_plt $@ --build_plt --apps $(PLT_APPS)

dialyzer: deps_plt
	dialyzer --fullpath --plt $^ -Wrace_conditions -r ebin

typer:
	typer -pa deps/lager -r src

clean:
	rebar skip_deps=true clean

clean_all:
	rebar skip_deps=true clean
	rm -f deps_plt
	rm -f README.html
	rm -f report.log
	rm -rf ebin/
	rm -rf doc/
	rm -rf log/
