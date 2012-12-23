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
       ?_test(run_something_t()),
       ?_test(multiple_t()),
       ?_test(crash_something_t()),
       ?_test(state_t()),
       ?_test(push_it_to_open_state_t())
     ]
    }.

start_stop_t() ->
    {ok, Pid} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, []}),

    %% Note: Pid is the pid of the circuit breaker. No process has been
    %% started yet.
    true = is_process_alive(Pid), 
    
    %% Stop it.
    breaky:stop_circuit_breaker(circuit1),
    false = is_process_alive(Pid).

run_something_t() ->
    {ok, _CircuitPid} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, ["data"]}),
    {ok, Pid} = breaky:pid(circuit1),
    true = is_process_alive(Pid),
    {ok, "data"} = gen_server:call(Pid, get_data),

    breaky:stop_circuit_breaker(circuit1),
    false = is_process_alive(Pid).

multiple_t() ->
    {ok, Pid1} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, ["data"]}),
    {ok, Pid2} = breaky:start_circuit_breaker(circuit2, 
        {breaky_test_server, start_link, ["data"]}),
    ?assert(Pid1 =/= Pid2),
    breaky:stop_circuit_breaker(circuit1),
    breaky:stop_circuit_breaker(circuit2).

crash_something_t() ->
    {ok, BPid} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, ["data"]}),

    %% Kill the process.
    {ok, Pid1} = breaky:pid(circuit1),

    closed = breaky:state(circuit1),

    exit(Pid1, brutal_kill),
    false = is_process_alive(Pid1),
    true = is_process_alive(BPid),

    closed = breaky:state(circuit1),

    %% Give it some time to restart.
    timer:sleep(100),

    %% Check if the process was restarted.
    {ok, Pid2} = breaky:pid(circuit1),
    ?assert(Pid1 =/= Pid2),
    {ok, "data"} = gen_server:call(Pid2, get_data),

    breaky:stop_circuit_breaker(circuit1).

state_t() ->
    {ok, BPid} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, ["data"]}),
    closed = breaky:state(circuit1),
    breaky:stop_circuit_breaker(circuit1),
    ok.


push_it_to_open_state_t() ->
    {ok, BPid} = breaky:start_circuit_breaker(circuit1, 
        {breaky_test_server, start_link, ["data"]}),
    closed = breaky:state(circuit1),

    %% 10 failures...
    repeat(fun() ->
            {ok, Pid} = breaky:pid(circuit1),
            exit(Pid, brutal_kill),
            timer:sleep(20)
           end, 10),

    %% It is in open state now... no pids
    open = breaky:state(circuit1),
    {error, open} = breaky:pid(circuit1),

    breaky:stop_circuit_breaker(circuit1),
    ok.

repeat(_F, 0) -> ok;
repeat(F, N) -> F(), repeat(F, N-1).