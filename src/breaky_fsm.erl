%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%%
%% @doc Erlang Circuit Breaker
%%
%% Copyright 2012 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(breaky_fsm).

-behaviour(gen_fsm).

%% 
-export([
    start_link/2,
    start_link/3,
    pid/1,
    state/1,
    failure/1,
    call/2, call/3,
    cast/2,
    whereis_name/1,
    send/2,
    fsm_pid/1
    ]).

%% gen_fsm callbacks
-export([
    init/1,
    initializing/2, initializing/3, 
    on/2, on/3, 
    off/2, off/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3, 
    terminate/3, 
    code_change/4
    ]).

-record(state, {
    failure_threshold=10,
    remainder_fails=10,

    retry_timeout=10000,
    retry_timer=undefined,

    name=undefined,
    sup=undefined,

    pid=undefined,
    ref=undefined
}).

-type name() :: atom() | {global, term()} | {via, module(), term()}.

%%
%% The circuit breaker has two states.
%%
%% 1) ``on``, everything is normal.
%% 2) ``off``, the process has failed to many times, after timeout the 
%%    switch will retry to start the process. If it fails it will move 
%%    back to off state, otherwise it will go to on state again.
%%

%% Start a circuit breaker.

start_link(Name, Spec) ->
    start_link(Name, self(), Spec).
start_link(Name, Sup, Spec) when is_atom(Name) ->
    start_link({local, Name}, Sup, Spec);
start_link(Name, Sup, Spec) ->
    gen_fsm:start_link(Name, ?MODULE, [Name, Sup, Spec], []).

% @doc Return the pid of the process we are managing.
%
-spec pid(name()) -> {ok, pid()} | off.
pid(Name) ->
    gen_fsm:sync_send_event(Name, pid).

% @doc Get the current state of the circuit breaker
%
-spec state(name()) -> on | off.
state(Name) ->
    gen_fsm:sync_send_all_state_event(Name, state).

% @doc Register something as a failure.
%
-spec failure(name()) -> ok.
failure(Name) ->
    gen_fsm:send_event(Name, failure).

% @doc Call service Name via the circuit braker.
call(Name, Msg) ->
    call(Name, Msg, 5000).

call(Name, Msg, Timeout) ->
    case gen_fsm:sync_send_event(Name, pid, Timeout) of
        off -> off;
        {ok, Pid} ->
            {ok, do_call(Pid, Msg, Timeout)}
    end.

do_call(Pid, Msg, Timeout) -> 
    gen_server:call(Pid, Msg, Timeout).

% @doc Cast service Name via the circuit braker.
cast(Name, Msg) ->
    case gen_fsm:sync_send_event(Name, pid) of
        off -> off;
        {ok, Pid} ->
            gen_server:cast(Pid, Msg)
    end.

%% Let the circuit breaker act as a process registry. Returns the
%% Pid when the process is running.
whereis_name(Name) ->
    case fsm_pid(Name) of
        undefined -> undefined;
        Pid -> 
            case gen_fsm:sync_send_event(Pid, pid) of
                {ok, Pid} -> 
                    Pid;
                off -> undefined
            end
    end.

% %% Let the circuit breaker act as a process registry. Returns the
% %% Pid when the process is running.
send(Name, Msg) ->
    case fsm_pid(Name) of
        undefined ->
            exit({badarg, Name, Msg});
        FsmPid ->
            case gen_fsm:sync_send_event(FsmPid, pid) of
                {ok, Pid} -> 
                    Pid ! Msg,
                    Pid;
                off -> 
                    exit({badarg, Name, Msg})
            end
    end.

%% Initialize the circuit breaker. It is in on state.
%% 
init([Name, Sup, Spec]) ->
    self() ! {initialize, Sup, Spec},
    {ok, initializing, #state{name=Name}}.

% @doc Not expecting any events in init state.
initializing(_Event, State) ->
    {stop, {error, not_initialized}, State}.
initializing(_Event, _From, State) ->
    {stop, {error, not_initialized}, State}.

% @doc The automatic circuit breaker is on
on(retry, State) ->
    error_msg("Received retry, but circuit breaker is on.", State),
    State1 = State#state{retry_timer=undefined},
    {next_state, on, State1};
on(failure, #state{remainder_fails=N}=State) when N > 1 ->
    {next_state, on, State#state{remainder_fails=N-1}};
on(failure, #state{sup=Sup, pid=Pid, remainder_fails=N}=State) when N =< 1 ->
    %% Too many failures. Move to off state... stop the process.
    case supervisor:terminate_child(Sup, Pid) of
        ok -> ok;
        {error, Err} ->
            error_msg("Problem terminating child: ~p", [Err], State)
    end,
    TimerRef = gen_fsm:send_event_after(State#state.retry_timeout, retry),
    {next_state, off, State#state{pid=undefined, retry_timer=TimerRef, remainder_fails=N-1}};
on(Event, State) -> 
    {stop, {error, {unknown_event, Event}}, State}.
on(pid, _From, #state{pid=Pid}=State) when is_pid(Pid) ->
    {reply, {ok, Pid}, on, State};
on(pid, _From, #state{pid=undefined}=State) ->
    %% The pid can be undefined when the process exited normally.
    {reply, undefined, on, State};
on(Event, _From, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

% @doc The circuit breaker is off.
%
off(retry, #state{failure_threshold=Max}=State) ->
    State1 = State#state{retry_timer=undefined},

    %% Restart the process...
    {NextState, NewState} = start_process(State1#state{remainder_fails=1}),
    NewState1 = case NextState of
        on -> NewState#state{remainder_fails=Max};
        _ -> NewState
    end,

    {next_state, NextState, NewState1};
off(failure, State) ->
    %% We have already failed.
    {next_state, off, State};
off(Event, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

off(pid, _From, State) ->
    {reply, off, off, State};
off(Event, _From, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

% @doc 
handle_event(_Msg, StateName, State) ->
    {next_state, StateName, State}.

% @doc Return the state of the circuit-breaker
handle_sync_event(state, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(_Msg, _From, StateName, State) ->
    {reply, ok, StateName, State}.

% @doc initialize. start the supervisor. 
handle_info({initialize, Sup, Spec}, initializing, State) ->
    ChildSpec = {breaky_fsm_sup, 
            {breaky_fsm_sup, start_link, [Spec]},
             permanent, 10000, supervisor, [breaky_fsm_sup]},
    {ok, SupPid} = supervisor:start_child(Sup, ChildSpec),
    {NextState, NewState} = start_process(State#state{sup=SupPid}),
    {next_state, NextState, NewState};    
handle_info({initialize, _Sup, _Spec}, _Other, State) ->
    {stop, {error, already_initialized}, State};

%% The process stopped normally, no state change.
handle_info({'DOWN', Ref, process, _Pid, normal}, StateName, #state{ref=Ref}=State) ->
    {next_state, StateName, State#state{pid=undefined, ref=undefined}};
% Failure.... restart? or move to off state?
% The process crashed... now what?
handle_info({'DOWN', Ref, process, _Pid, Reason}, on, #state{ref=Ref, remainder_fails=R}=State) ->
    error_msg("Process down: ~p", [Reason], State),
    {NextState, NewState} = start_process(State#state{pid=undefined, ref=undefined, remainder_fails=R-1}),
    {next_state, NextState, NewState};

handle_info(_Msg, StateName, #state{}=State) ->
    {next_state, StateName, State}.

% @doc Terminate the fsm. 
terminate(_Reason, _StateName, _State) ->
    % the supervisor will stop the process.
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% -- utility functions

% start the process.
start_process(#state{sup=Sup, remainder_fails=N}=State) when N > 0->
    case supervisor:start_child(Sup, []) of
        {ok, Pid} ->
            NewRef = erlang:monitor(process, Pid),
            {on, State#state{ref=NewRef, pid=Pid}};
        {ok, Pid, _Info} ->
            NewRef = erlang:monitor(process, Pid),
            {on, State#state{ref=NewRef, pid=Pid}};
        {error, Error} ->
            error_msg("Could not start process: ~p", [Error], State),
            start_process(State#state{remainder_fails=N-1})
    end;
start_process(#state{remainder_fails=N}=State) when N =< 0 ->
    error_msg("Too many failures, switching to off state.", State),
    TimerRef = gen_fsm:send_event_after(State#state.retry_timeout, retry),
    {off, State#state{retry_timer=TimerRef, remainder_fails=0}}.

% Return the pid of the breaker fsm
-spec fsm_pid(name()) -> undefined | pid().
fsm_pid(Name) when is_atom(Name) ->
    whereis(Name);
fsm_pid({global, Name}) ->
    global:whereis_name(Name);
fsm_pid({via, Module, Name}) ->
    Module:whereis_name(Name).

% Log errors.
error_msg(Msg, #state{name=Name}) ->
    error_logger:error_msg("Circuit breaker ~p: ~s", [Name, Msg]).

error_msg(Msg, Args, #state{name=Name}) ->
    error_logger:error_msg("Circuit breaker ~p: "++Msg, [Name | Args]).
