.PHONY: all compile doc run test clean

SRC  = $(wildcard src/*.cpp)
OBJ  = $(patsubst %.cpp,_build/%.o,$(SRC))
DEPS = $(patsubst %.o,%.d,$(OBJ))

INC_DIRS = include $(ERL_INTERFACE)/include
LIB_DIRS = $(ERL_INTERFACE)/lib
LIBS     = erl_interface ei
CXXFLAGS = -c -MMD -MP -std=c++1y -pthread
# CXXFLAGS = -c -MMD -MP -std=c++1y -pthread -O2 -DNDEBUG
LDFLAGS  = -pthread


SRC_TEST     = $(wildcard test/*.cpp)
OBJ_NOMAIN   = $(subst src,test,$(filter-out %Main.o,$(OBJ)))
OBJ_TEST     = $(patsubst %.cpp,_build/%.o,$(SRC_TEST))
OBJ_TEST_ALL = $(OBJ_TEST) $(OBJ_NOMAIN)
DEPS_TEST    = $(patsubst %.o,%.d,$(OBJ_TEST_ALL))

CXXFLAGS_TEST=-c -g -MMD -MP -std=c++1y -pthread
LIBS_TEST=$(LIBS) boost_unit_test_framework

SET_ERL_INTERFACE = $(if $(ERL_INTERFACE),,$(eval ERL_INTERFACE := $(shell erl -noinput -eval 'io:format (code:lib_dir(erl_interface)).' -s init stop)))

all: compile

compile: _build/compute_node

_build/compute_node: $(OBJ)
	$(call SET_ERL_INTERFACE)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIB_DIRS:%=-L%) $(LIBS:%=-l%)

_build/src/%.o: src/%.cpp
	@mkdir -p $(@D)
	$(call SET_ERL_INTERFACE)
	$(CXX) $(CXXFLAGS) -o $@ $< $(INC_DIRS:%=-I%)

doc:
	@mkdir -p _build
	doxygen

test: _build/unit_test
	_build/unit_test --log_level=message --run_test=$(TEST_CASE)

_build/unit_test: $(OBJ_TEST_ALL)
	$(call SET_ERL_INTERFACE)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIB_DIRS:%=-L%) $(LIBS_TEST:%=-l%)

$(OBJ_TEST): _build/test/%.o: test/%.cpp

$(OBJ_NOMAIN): _build/test/%.o: src/%.cpp

$(OBJ_TEST_ALL):
	@mkdir -p $(@D)
	$(call SET_ERL_INTERFACE)
	$(CXX) $(CXXFLAGS_TEST) -o $@ $< $(INC_DIRS:%=-I%)

run: compile $(HOME)/.erlang.cookie
	./_build/compute_node&

$(HOME)/.erlang.cookie:
	erl -noinput -sname visual_nn -s init stop

clean:
	rm -rf _build

-include $(DEPS) $(DEPS_TEST)
