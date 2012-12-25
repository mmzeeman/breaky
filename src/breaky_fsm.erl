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
    call/2, call/3,
    cast/2,
    whereis_name/1,
    send/2
    ]).

%% gen_fsm callbacks
-export([
    init/1,
    initializing/2, initializing/3, 
    closed/2, closed/3, 
    open/2, open/3,
%   reset/0,
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

%%
%% The circuit breaker has two states.
%%
%% 1) ``closed``, everything is normal.
%% 2) ``open``, the process has failed to many times, after timeout the 
%%    switch will retry to start the process. If it fails it will move 
%%    back to open state, otherwise it will go to closed state again.
%%

%% Start a circuit breaker.

%% TODO: support for global and via
start_link(Name, MFA) ->
    start_link(Name, self(), MFA).
start_link(Name, Sup, MFA) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Name, Sup, MFA], []).

% @doc Return the pid of the process we are managing.
%
-spec pid(Name) -> {ok, pid()} | open when
    Name :: atom() | {global, term()} | {via, module(), term()}.
pid(Name) ->
    gen_fsm:sync_send_event(Name, pid).

% @doc get the current state of the circuit breaker
%
-spec state(Name) -> closed | open when
    Name :: atom() | {global, term()} | {via, module(), term()}.
state(Name) ->
    gen_fsm:sync_send_all_state_event(Name, state).

% This w
call(Name, Msg) ->
    call(Name, Msg, 5000).

call(Name, Msg, infinity) ->
    case gen_fsm:sync_send_event(Name, pid, infinity) of
        open -> open;
        {ok, Pid} ->
            {ok, do_call(Name, Pid, Msg, infinity)}
    end;
call(Name, Msg, Timeout) ->
    %% TODO: calculate the right timeout here.
    case gen_fsm:sync_send_event(Name, pid, Timeout) of
        open -> open;
        {ok, Pid} ->
            {ok, do_call(Name, Pid, Msg, Timeout)}
    end.

do_call(Name, Pid, Msg, Timeout) ->
    try gen_server:call(Pid, Msg, Timeout) of
        Result -> Result
    catch
        Exception ->
            gen_fsm:send_event(Name, failure),
            throw(Exception)
    end.

cast(Name, Msg) ->
    case gen_fsm:sync_send_event(Name, pid) of
        open -> open;
        {ok, Pid} ->
            {ok, gen_server:cast(Pid, Msg)}
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
                open -> undefined
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
                open -> 
                    exit({badarg, Name, Msg})
            end
    end.

%% Initialize the circuit breaker. It is in closed state.
%% 
init([Name, Sup, MFA]) ->
    self() ! {initialize, Sup, MFA},
    {ok, initializing, #state{name=Name}}.

% @doc Not expecting any events in init state.
initializing(_Event, State) ->
    {stop, {error, not_initialized}, State}.
initializing(_Event, _From, State) ->
    {stop, {error, not_initialized}, State}.

% @doc The automatic circuit breaker is closed
closed(retry, State) ->
    error_logger:error_msg("Received retry, but circuit breaker is closed.~n", []),
    State1 = State#state{retry_timer=undefined},
    {next_state, closed, State1};
closed(failure, #state{remainder_fails=N}=State) when N > 1 ->
    {next_state, closed, State#state{remainder_fails=N-1}};
closed(failure, #state{sup=Sup, pid=Pid, remainder_fails=N}=State) when N =< 1 ->
    %% Too many failures. Move to open state... stop the process.
    case supervisor:terminate_child(Sup, Pid) of
        ok -> ok;
        {error, Err} ->
            error_logger:error_msg("Problem terminating child: ~p~n", [Err])
    end,
    TimerRef = gen_fsm:send_event_after(State#state.retry_timeout, retry),
    {next_state, open, State#state{pid=undefined, retry_timer=TimerRef, remainder_fails=N-1}};
closed(Event, State) -> 
    {stop, {error, {unknown_event, Event}}, State}.

closed(pid, _From, #state{pid=Pid}=State) when is_pid(Pid) ->
    {reply, {ok, Pid}, closed, State};
closed(pid, _From, #state{pid=undefined}) ->
    %% we should have a pid in closed state.
    exit(pid_undefined);
closed(Event, _From, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

% @doc The circuit breaker is open.
%
open(retry, #state{failure_threshold=Max}=State) ->
    State1 = State#state{retry_timer=undefined},

    %% Restart the process...
    {NextState, NewState} = start_process(State1#state{remainder_fails=1}),
    NewState1 = case NextState of
        closed -> NewState#state{remainder_fails=Max};
        _ -> NewState
    end,

    {next_state, NextState, NewState1};
open(failure, State) ->
    %% We have already failed.
    {next_state, open, State};
open(Event, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

open(pid, _From, State) ->
    {reply, open, open, State};
open(Event, _From, State) ->
    {stop, {error, {unknown_event, Event}}, State}.

% @doc 
handle_event(_Msg, StateName, State) ->
    {next_state, StateName, State}.

% @doc 
handle_sync_event(state, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(_Msg, _From, StateName, State) ->
    {reply, ok, StateName, State}.

% @doc initialize. start the supervisor. 
handle_info({initialize, Sup, MFA}, initializing, State) ->
    Spec = {breaky_fsm_sup,
            {breaky_fsm_sup, start_link, [MFA]},
             permanent, 10000, supervisor, [breaky_fsm_sup]},
    {ok, SupPid} = supervisor:start_child(Sup, Spec),
    {NextState, NewState} = start_process(State#state{sup=SupPid}),
    {next_state, NextState, NewState};    
handle_info({initialize, _Sup, _MFA}, _Other, State) ->
    {stop, {error, already_initialized}, State};

%% The process stopped normally. 
handle_info({'DOWN', Ref, process, _Pid, normal}, StateName, #state{ref=Ref}=State) ->
    {next_state, StateName, State#state{pid=undefined, ref=undefined}};

% Failure.... restart? or move to open state?
handle_info({'DOWN', Ref, process, _Pid, Reason}, closed, #state{ref=Ref, remainder_fails=R}=State) ->
    error_logger:error_msg("Process failed: ~p~n", [Reason]),
    {NextState, NewState} = start_process(State#state{pid=undefined, ref=undefined, remainder_fails=R-1}),
    {next_state, NextState, NewState};

handle_info(_Msg, StateName, #state{}=State) ->
    {next_state, StateName, State}.

% @doc 
terminate(_Reason, _StateName, _State) ->
    ok.

% @doc
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

% utility functions

start_process(#state{sup=Sup, remainder_fails=N}=State) when N > 0->
    case supervisor:start_child(Sup, []) of
        {ok, Pid} ->
            NewRef = erlang:monitor(process, Pid),
            {closed, State#state{ref=NewRef, pid=Pid}};
        {ok, Pid, _Info} ->
            NewRef = erlang:monitor(process, Pid),
            {closed, State#state{ref=NewRef, pid=Pid}};
        {error, Error} ->
            error_logger:error_msg("Circuit breaker could not start process: ~p~n", [Error]),
            start_process(State#state{remainder_fails=N-1})
    end;
start_process(#state{remainder_fails=N}=State) when N =< 0 ->
    error_logger:error_msg("Too many failures, switching to open state~n", []),
    TimerRef = gen_fsm:send_event_after(State#state.retry_timeout, retry),
    {open, State#state{retry_timer=TimerRef, remainder_fails=0}}.

%
fsm_pid(Name) when is_atom(Name) ->
    whereis(Name);
fsm_pid({global, Name}) ->
    global:whereis_name(Name);
fsm_pid({via, Module, Name}) ->
    Module:whereis_name(Name).