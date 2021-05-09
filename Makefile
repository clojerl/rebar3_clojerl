.PHONY: compile clean docs publish

compile:
	@ echo "Compiling..."
	@ rebar3 compile

clean:
	@ echo "Cleaning..."
	@ rebar3 clean
	@ rm -rf _build ebin

docs:
	@ echo "Generating docs..."
	@ rebar3 docs

publish: clean compile docs
	@ echo "Publishing..."
	@ rebar3 hex publish
