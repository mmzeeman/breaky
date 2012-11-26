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

%%% The top-level supervisor of the registration
%%% server.

-module(fusebox_app_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_fuse/1, stop_fuse/1]).

-define(TABLE, fusebox).

% @doc
start_link() ->
    supervisor:start_link({local, fusebox_app_sup}, ?MODULE, []).

% @doc
init([]) ->
	ets:new(?TABLE, [public, set, named_table]),
    {ok, {{one_for_one, 1, 3600}, []}}.

% @doc Start a fuse
start_fuse(Name) ->
    ChildSpec = {Name,
        {fuse_sup, start_link, [Name, ?TABLE]},
        permanent, 10000, supervisor, [fuse_sup, fuse, fuse_subject_sup]},
    {ok, _Pid} = supervisor:start_child(fusebox_app_sup, ChildSpec).

% @doc Stop 
stop_fuse(_Name) ->
    ok.

% @doc Call an MFA, and check timeout's and crashes
call(Name, MFA) ->
    case ets:lookup(?TABLE, Name) of
        [Name, Pid, StateName] ->
        	fuse:call(Pid, State, MFA);
        [] ->
        	{error, unknown_fuse} 
    end.
    