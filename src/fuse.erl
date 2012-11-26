%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%%
%% @doc Erlang Fusebox
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

-module(fuse).

-behaviour(gen_fsm).

%% 
-export([
	start_link/3
	]).

%% gen_fsm callbacks
-export([
	init/1, 
	closed/2, closed/3, 
	open/2, open/3,
	half_open/2,  half_open/3,
%	reset/0,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3, 
	terminate/3, 
	code_change/4
	]).

-export([call/3]).


-record(state, {
	threshold=10,
	remainder_fails=10,
	attempt_timeout=10000,
	timeout_timer=undefined,

    name=undefined,
    subject_sup=undefined,
    table=undefined
}).

%% Start a fuse.
start_link(Name, Sup, Table) ->
    gen_fsm:start_link({local, Name}, ?MODULE, [Name, Sup, Table], []).

%% Initialize the fuse. It is in closed state.
%% 
%% TODO: create ets table for easy reading of "current" state.
init([Name, Sup, Table]) ->
    self() ! {start_subject_supervisor, Sup},

    %% TODO: read current state from the table...
    true = ets:insert(Table, {Name, self(), closed}),
    {ok, closed, #state{name=Name, table=Table}}.

% @doc The automatic fuse is closed
closed(_Event, State) ->
	{next_state, closed, State}.

% @doc The automatic fuse is closed
closed(_Event, _From, State) ->
	{next_state, closed, State}.

% @doc The automatic fuse is half open
half_open(_Event, State) ->
    {next_state, half_open, State}.
    
% @doc 
half_open(_Event, _From, State) ->
    {next_state, half_open, State}.

% @doc The fuse is open.
open(_Event, State) ->
    {next_state, open, State}.

open(_Event, _From, State) ->
    {next_state, open, State}.

% @doc 
handle_event(_Msg, closed, State) ->
    {next_state, closed, State};
handle_event(_Msg, half_open, State) ->
    {next_state, half_open, State};
handle_event(_Msg, open, State) ->
    {next_state, open, State}.

% @doc 
handle_sync_event(_Msg, _From, closed, State) ->
    {reply, ok, closed, State};
handle_sync_event(_Msg, _From, half_open, State) ->
    {reply, ok, half_open, State};
handle_sync_event(_Msg, _From, open, State) ->
    {reply, ok, open, State}.

% @doc 
handle_info({start_subject_supervisor, Sup}, StateName, State) ->
    Spec = {fuse_subject_sup,
            {fuse_subject_sup, start_link, []},
             permanent, 10000, supervisor, [fuse_subject_sup]},
    Pid = ensure_supervisor(Sup, Spec),
    {next_state, StateName, State#state{subject_sup=Pid}};
handle_info(_Msg, closed, State) ->
    {next_state, closed, State};
handle_info(_Msg, half_open, State) ->
    {next_state, half_open, State};
handle_info(_Msg, open, State) ->
    {next_state, open, State}.

% @doc 
terminate(_Reason, _StateName, _State) ->
	ok.

% @doc
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

ensure_supervisor(Sup, Spec) ->
    case supervisor:start_child(Sup, Spec) of
        {ok, Pid} -> 
            Pid;
        {error, {already_started, Pid}} -> 
            Pid
    end. 
    

%%

call(FusePid, closed, {M, F, A}) ->
    %% everything is safe, call the MFA, but check for errors 
    %% and timeouts.
    Ref = erlang:make_ref(),
    Pid = self(),
    F = fun() ->
        try
            Pid ! {ok, Ref, erlang:apply(M, F, A)}
        catch
            Exception ->
                Pid ! {throw, Ref, Exception}
        end        
    end,
    erlang:spawn(F),
    receive
        {ok, Ref, Term} ->
            Term;
        {throw, Ref, Exception} ->
            gen_fsm:exception(FusePid),
            throw(Exception)
    end;
call(Pid, half_open, {M, F, A}) ->
    
call(Pid, open, {_M, _F, _A}) ->
    throw(open).
    

