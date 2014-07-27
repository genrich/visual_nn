.PHONY: all doc readme compile shell report run run_compute_node run_visual_nn run_debug run_debug_visual_nn test test_shaders dialyzer typer clean clean_all

DEPS     = goldrush lager ibrowse yaws
PLT_APPS = erts kernel stdlib $(DEPS)

PA_DEPS_EBIN = $(DEPS:%=-pa deps/%/ebin)
EBINS        = $(PA_DEPS_EBIN) -pa ../visual_nn/ebin

SHADER_TESTS = $(patsubst test/shader/%.cpp, test/shader/_build/%.test, $(wildcard test/shader/*.cpp))
SHADER_DEPS  = $(patsubst %.test, %.d, $(SHADER_TESTS))

CN_OBJ      = $(patsubst compute_node/src/%.cpp, compute_node/_build/%.o, $(wildcard compute_node/src/*.cpp))
CN_DEPS     = $(patsubst %.o, %.d, $(CN_OBJ))
CN_INC_DIRS = compute_node/include $(ERL_INTERFACE)/include
CN_LIB_DIRS = $(ERL_INTERFACE)/lib
CN_LIBS     = erl_interface ei
CN_CXXFLAGS = -c -MMD -MP -std=c++1y -pthread -O2 -fdiagnostics-color=auto
CN_LDFLAGS  = -pthread

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

compile: deps/yaws www/js/Const.js compute_node/_build compute_node/_build/compute_node
	@rebar skip_deps=true compile

compute_node/_build:
	mkdir -p $@

compute_node/_build/%.o: compute_node/src/%.cpp
	$(if $(ERL_INTERFACE), , $(eval ERL_INTERFACE := $(shell erl -noinput -eval 'io:format (code:lib_dir(erl_interface)).' -s init stop)))
	$(CXX) $(CN_CXXFLAGS) -o $@ $< $(CN_INC_DIRS:%=-I%)

compute_node/_build/compute_node: $(CN_OBJ)
	$(if $(ERL_INTERFACE), , $(eval ERL_INTERFACE := $(shell erl -noinput -eval 'io:format (code:lib_dir(erl_interface)).' -s init stop)))
	$(CXX) $(CN_LDFLAGS) -o $@ $^ $(CN_LIB_DIRS:%=-L%) $(CN_LIBS:%=-l%)

report: deps/yaws
	@sed -i 's/%% report,/report,/' rebar.config
	@rebar skip_deps=true clean compile
	@sed -i 's/report,/%% report,/' rebar.config

shell: compile
	erl $(EBINS)

$(HOME)/.erlang.cookie:
	erl -noinput -sname visual_nn -s init stop

run_compute_node: compile $(HOME)/.erlang.cookie
	./compute_node/_build/compute_node&

run_visual_nn: compile
	erl $(EBINS) -sname visual_nn -eval 'application:start (visual_nn).'

run: run_compute_node run_visual_nn 

run_debug_visual_nn: compile
	erl $(EBINS) -sname visual_nn -eval 'application:ensure_all_started (lager), lager:set_loglevel (lager_console_backend, debug), application:start (visual_nn).'

run_debug: run_compute_node run_debug_visual_nn

test:
	@rebar skip_deps=true eunit $(TEST_CASE)

test/shader/_build:
	mkdir -p $@

test/shader/_build/%.test: test/shader/%.cpp
	$(CXX) -g -MMD -MP -std=c++1y -fdiagnostics-color=auto -fno-operator-names -o $@ $<

test_shaders: test/shader/_build $(SHADER_TESTS)
	@for i in $$(find test/shader/_build -name *.test -type f); do $$i; if [[ $$? -ne 0 ]]; then exit 1; fi; done; echo 'shaders OK.'

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
	rm -rf compute_node/_build test/shader/_build

clean_all: clean
	rm -f deps_plt README.html report.log
	rm -rf ebin/ deps/ doc/ log/ priv/

-include $(SHADER_DEPS) $(CN_DEPS)
