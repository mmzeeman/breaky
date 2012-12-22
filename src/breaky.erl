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

-module(breaky).

-export([start_circuit_breaker/2, stop_circuit_breaker/1]).
-export([pid/1, call/2, call/3, cast/2]).

% @doc Start a new circuit breaker.
%
-spec start_circuit_breaker(Name, MFA) -> {ok, pid()} | {error, _} | ignore when
    Name :: atom(),
	MFA :: mfa().
start_circuit_breaker(Name, MFA) ->
    breaky_app_sup:start(Name, MFA).

% @doc Stop the circuit breaker with Name. 
%
-spec stop_circuit_breaker(Name) -> ok | {error, _} when
    Name :: atom().
stop_circuit_breaker(Name) ->
    breaky_app_sup:stop(Name).

% @doc Get the pid of the process managed by the supervisor
%
-spec pid(Name) -> {ok, pid()} | {error, _} when
	Name :: atom().
pid(Name) ->
	breaky_break:pid(Name).

% @doc Call the process
%
call(Name, Msg) ->
	call(Name, Msg, infinity).
call(Name, Msg, Timeout) ->
	breaky_break:call(Name, Msg, Timeout).

% @doc Cast the process 
%
cast(Name, Msg) ->
	breaky_break:cast(Name, Msg).
