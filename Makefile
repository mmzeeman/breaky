PROJECT = breaky
REBAR := $(shell which rebar 2>/dev/null || echo ./rebar)
REBAR_URL := https://github.com/downloads/basho/rebar/rebar
DIALYZER = dialyzer

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
        -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
	
compile: rebar
	$(REBAR) compile

eunit: rebar
	$(REBAR) compile
	$(REBAR) eunit -v skip_deps=true

clean: rebar
	$(REBAR) clean

distclean: 
	rm $(REBAR)


# dializer 

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib 

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs
