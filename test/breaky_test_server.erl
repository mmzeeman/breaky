%%
%% Small test server
%% 

-module(breaky_test_server).
-behaviour(gen_server).

%% gen_server callback api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([start_link/1]).

-record(state, {data}).

start_link(Data) ->
	gen_server:start_link(?MODULE, Data, []).

init(Data) ->
	{ok, #state{data=Data}}.

handle_call(get_data, _From, State) ->
	{reply, {ok, State#state.data}, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Msg, _From, State) ->
	{stop, {unknown_cast, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.