.PHONY: all doc readme compile compile_test_shaders shell report run run_debug test test_shaders dialyzer typer clean clean_all

CXXFLAGS+=-g -MMD -MP -std=c++1y -fdiagnostics-color=auto -fno-operator-names

DEPS=goldrush lager ibrowse yaws
PLT_APPS=erts kernel stdlib $(DEPS)

PA_DEPS_EBIN=$(DEPS:%=-pa deps/%/ebin)
EBINS=$(PA_DEPS_EBIN) -pa ../visual_nn/ebin

SHADER_TESTS=$(patsubst test/shader/%.cpp, test/shader/bin/%.test, $(wildcard test/shader/*.cpp))
SHADER_DEPS=$(patsubst %.test, %.d, $(SHADER_TESTS))

all: compile

doc:
	rebar skip_deps=true doc

readme:
	pandoc README.md -o README.html

deps/yaws:
	rebar get-deps compile

www/js/Const.js: include/const.hrl
	@awk   'BEGIN 		    { print "var CONST =\n{" };                                                                    \
			$$1 ~ /^$$/     { print };                                                                                     \
			$$1 ~ /%%/      { $$1 = "    //"; print $$0 };                                                                 \
			$$1 ~ /-define/ { $$1 = "   "; gsub ("[()]", ""); sub (",", ":"); sub ("\\.(\\s*%(.*))?$$", ","); print $$0 }; \
			END             { print "}" }'                                                                                 \
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
	erl $(EBINS) -eval 'application:ensure_all_started (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

test:
	@rebar skip_deps=true eunit $(TEST_CASE)

test/shader/bin:
	mkdir -p $@

test/shader/bin/%.test: test/shader/%.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

compile_test_shaders: test/shader/bin $(SHADER_TESTS)

test_shaders: compile_test_shaders
	@for i in $$(find test/shader/bin -name *.test -type f); do $$i; if [[ $$? -ne 0 ]]; then exit 1; fi; done; echo 'shaders OK.'

-include $(SHADER_DEPS)

deps_plt: deps/yaws
	sed -i 's/no_debug_info/debug_info/' deps/yaws/rebar.config
	rebar -r clean compile
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
	rm -rf test/shader/bin

clean_all: clean
	rm -f deps_plt README.html report.log
	rm -rf ebin/ deps/ doc/ log/ priv/
