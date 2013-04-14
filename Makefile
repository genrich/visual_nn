.PHONY: build run

build: deps/yaws
	rebar compile

clean:
	rebar clean

deps/yaws:
	rebar get-deps

run: build
	erl -pa deps/ibrowse/ebin/ -pa deps/lager/ebin/ -pa deps/yaws/ebin/ -pa ebin/ -eval 'application:start (visual_nn).'
