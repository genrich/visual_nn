.PHONY: all doc readme compile shell report run run_compute_node run_visual_nn run_debug run_debug_visual_nn test test_shaders dialyzer typer clean clean_all

DEPS     = goldrush lager ibrowse yaws
PLT_APPS = erts kernel stdlib $(DEPS)

PA_DEPS_EBIN = $(DEPS:%=-pa deps/%/ebin)
EBINS        = $(PA_DEPS_EBIN) -pa ../visual_nn/ebin

SHADER_TESTS = $(patsubst test/shader/%.cpp, test/shader/_build/%.test, $(wildcard test/shader/*.cpp))
SHADER_DEPS  = $(patsubst %.test, %.d, $(SHADER_TESTS))

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

compute_node/_build/compute_node: compute_node/src/*.cpp compute_node/include/*.hpp
	$(MAKE) -C compute_node compile

compile: deps/yaws www/js/Const.js compute_node/_build/compute_node
	@rebar skip_deps=true compile

report: deps/yaws
	@sed -i 's/%% report,/report,/' rebar.config
	@rebar skip_deps=true clean compile
	@sed -i 's/report,/%% report,/' rebar.config

shell: compile
	erl $(EBINS)

run_compute_node: compile
	$(MAKE) -C compute_node run

run_visual_nn: compile
	erl $(EBINS) -sname visual_nn -eval 'application:start (visual_nn).'

run: run_compute_node run_visual_nn 

run_debug_visual_nn: compile
	erl $(EBINS) -sname visual_nn -eval 'application:ensure_all_started (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

run_debug: run_compute_node run_debug_visual_nn

test:
	@rebar skip_deps=true compile eunit $(TEST_CASE)

test/shader/_build:
	mkdir -p $@

test/shader/_build/%.test: test/shader/%.cpp
	$(CXX) -g -MMD -MP -std=c++1y -fdiagnostics-color=auto -fno-operator-names -o $@ $<

test_shaders: test/shader/_build $(SHADER_TESTS)
	@for i in $$(find test/shader/_build -name *.test -type f); do $$i; if [[ $$? -ne 0 ]]; then exit 1; fi; done; echo 'shaders OK.'

deps_plt: deps/yaws
	sed -i 's/no_debug_info/debug_info/' deps/yaws/rebar.config
	rebar -r clean compile
	dialyzer $(PA_DEPS_EBIN) --output_plt $@ --build_plt --apps $(PLT_APPS)

dialyzer: deps_plt
	rebar compile
	dialyzer --fullpath --plt $^ -Wrace_conditions -r ebin

typer:
	typer -pa deps/lager -r src

clean:
	rebar skip_deps=true clean
	$(MAKE) -C compute_node clean
	rm -rf test/shader/_build

clean_all: clean
	rm -f deps_plt README.html report.log
	rm -rf ebin/ deps/ doc/ log/ priv/

-include $(SHADER_DEPS)
