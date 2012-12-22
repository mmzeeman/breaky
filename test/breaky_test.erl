%% Some tests

-module(breaky_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:start(breaky).

teardown(_) ->
    application:stop(breaky).

application_start_stop_test() ->
    ?assertEqual(ok, setup()),
    ?assertEqual(ok, teardown([])).

breaky_test_() ->
    {foreach, local, fun setup/0, fun teardown/1,
     [ ?_test(start_stop_t()),
       ?_test(run_something_t())
     ]
    }.

start_stop_t() ->
	{ok, Pid} = breaky:start_circuit_breaker(circuit1, {breaky_test_server, start_link, []}),
	
	%% Note: Pid is the pid of the circuit breaker. No process has been
	%% started yet.
	true = is_process_alive(Pid), 
	
	%% Stop it.
	breaky:stop_circuit_breaker(circuit1),

	false = is_process_alive(Pid).

run_something_t() ->
	{ok, _CircuitPid} = breaky:start_circuit_breaker(circuit1, 
		{breaky_test_server, start_link, []}),

	{ok, Pid} = breaky:run(circuit1, ["data"]),
	true = is_process_alive(Pid),

	breaky:stop_circuit_breaker(circuit1),
	false = is_process_alive(Pid).

