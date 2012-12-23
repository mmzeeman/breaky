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

-module(breaky_fsm_sup).

-export([start_link/1, init/1]).
-behaviour(supervisor).

% @doc 
-spec start_link(MFA) -> {ok, pid()} | {error, _} | ignore when
    MFA :: mfa().
start_link(MFA) ->
    supervisor:start_link(?MODULE, MFA).

% @doc Start a supervisor for the breaker. 
%
init({Module, Function, Args}) ->
    % TODO: add an option to configure a task supervisor which
    % can restart tasks if they fail.

    %% TODO, support starting supervisors... and pass deps.
    {ok, {{simple_one_for_one, 10, 60},
          [{breaky_proc, {Module, Function, Args},
            temporary, 5000, worker, [Module]}]}}.
