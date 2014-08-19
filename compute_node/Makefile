.PHONY: all compile run test clean clean_all

OBJ  = $(patsubst src/%.cpp, _build/%.o, $(wildcard src/*.cpp))
DEPS = $(patsubst %.o, %.d, $(OBJ))

INC_DIRS = include $(ERL_INTERFACE)/include
LIB_DIRS = $(ERL_INTERFACE)/lib
LIBS     = erl_interface ei
CXXFLAGS = -c -MMD -MP -std=c++1y -pthread -fdiagnostics-color=auto -O2 -DNDEBUG
LDFLAGS  = -pthread

CXXFLAGS_TEST=-std=c++1y -g
LIBS_TEST=$(LIBS) boost_unit_test_framework

SET_ERL_INTERFACE = $(if $(ERL_INTERFACE), , $(eval ERL_INTERFACE := $(shell erl -noinput -eval 'io:format (code:lib_dir(erl_interface)).' -s init stop)))

all: compile

compile: _build _build/compute_node

_build:
	mkdir -p $@

_build/%.o: src/%.cpp
	$(call SET_ERL_INTERFACE)
	$(CXX) $(CXXFLAGS) -o $@ $< $(INC_DIRS:%=-I%)

_build/compute_node: $(OBJ)
	$(call SET_ERL_INTERFACE)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIB_DIRS:%=-L%) $(LIBS:%=-l%)

$(HOME)/.erlang.cookie:
	erl -noinput -sname visual_nn -s init stop

run: compile $(HOME)/.erlang.cookie
	./_build/compute_node&

_build/test: test/*.cpp src/Network.cpp
	$(call SET_ERL_INTERFACE)
	$(CXX) $(CXXFLAGS_TEST) -o $@ $^ $(INC_DIRS:%=-I%) $(LIB_DIRS:%=-L%) $(LIBS_TEST:%=-l%)

test: _build _build/test
	_build/test --log_level=message --run_test=$(TEST_CASE)

clean:
	rm -rf _build

-include $(DEPS)
