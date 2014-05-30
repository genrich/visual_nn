.PHONY: all doc readme compile shell report run run_debug test dialyzer typer clean clean_all

DEPS=goldrush lager ibrowse yaws
PLT_APPS=erts kernel stdlib $(DEPS)

PA_DEPS_EBIN=$(DEPS:%=-pa deps/%/ebin)
EBINS=$(PA_DEPS_EBIN) -pa ../visual_nn/ebin

all: compile

doc:
	rebar skip_deps=true doc

readme:
	pandoc README.md -o README.html

deps/yaws:
	rebar get-deps compile

www/js/Const.js: include/const.hrl
	@awk   'BEGIN 		    { print "var CONST =\n{" };                                                      \
			$$1 ~ /^$$/     { print };                                                                       \
			$$1 ~ /%%/      { $$1 = "    //"; print $$0 };                                                   \
			$$1 ~ /-define/ { $$1 = "   "; gsub ("[()]", ""); sub (",", ":"); sub ("\\.", ","); print $$0 }; \
			END 			{ print "}" }'                                                                   \
	$^ > $@

compile: deps/yaws www/js/Const.js
	@rebar skip_deps=true compile

report: deps/yaws
	@sed -i 's/%% report,/report,/' rebar.config
	@rebar skip_deps=true clean compile
	@sed -i 's/report,/%% report,/' rebar.config

shell: compile
	erl $(EBINS)

run: compile
	erl $(EBINS) -eval 'application:start (visual_nn).'

run_debug: compile
	erl $(EBINS) -eval 'application:start (syntax_tools), application:start (compiler), application:start (goldrush), application:start (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

test:
	@rebar skip_deps=true eunit $(TEST_CASE)

deps_plt: deps/yaws
	sed -i 's/no_debug_info/debug_info/' deps/yaws/rebar.config
	rebar clean compile
	rm -f deps/yaws/ebin/mime_type_c.beam
	rm -f deps/yaws/ebin/yaws_generated.beam
	dialyzer $(PA_DEPS_EBIN) --output_plt $@ --build_plt --apps $(PLT_APPS)

dialyzer: deps_plt
	rebar compile
	dialyzer --fullpath --plt $^ -Wrace_conditions -r ebin

typer:
	typer -pa deps/lager -r src

clean:
	rebar skip_deps=true clean

clean_all: clean
	rm -f deps_plt README.html report.log
	rm -rf ebin/ deps/ doc/ log/ priv/
