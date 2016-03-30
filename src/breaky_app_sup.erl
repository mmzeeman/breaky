%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%%
%% @doc Erlang Circuit Breaker
%%
%% Copyright 2012, 2015 Maas-Maarten Zeeman
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

%%% The top-level supervisor of the registration
%%% server.

-module(breaky_app_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

%% Api

-export([
    start/3, start/4,
    stop/1, stop/2
]).

% @doc
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc
init([]) ->
    {ok, {{one_for_one, 1, 3600}, []}}.

% @doc Start a circuit breaker
start(Name, MFA, Opts) ->
    start(?MODULE, Name, MFA, Opts).

% @doc Start as child of another supervisor circuit breaker
start(Supervisor, Name, MFA, Opts) ->
    ChildSpec = {Name,
        {breaky_sup, start_link, [Name, MFA, Opts]},
        permanent, 10000, supervisor, [breaky_sup, breaky_fsm, breaky_fsm_sup]},
    supervisor:start_child(Supervisor, ChildSpec).

% @doc Stop the circuit breaker
stop(Name) ->
    stop(?MODULE, Name).

% @doc Stop the circuit breaker.
stop(Supervisor, Name) ->
    case supervisor:terminate_child(Supervisor, Name) of
        ok -> 
            supervisor:delete_child(Supervisor, Name);
        {error, _}=Error -> 
            Error
    end.
