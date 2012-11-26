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

-module(fusebox).

-export([
	start_fuse/1, 
	stop_fuse/1,

	call/4
	]).

% @doc Start a new fusebox.
start_fuse(Name) ->
    fusebox_app_sup:start_fuse(Name).

% @doc Stop the Named fusebox.
stop_fuse(Name) ->
    fusebox_app_sup:stop_fuse(Name).

% @doc
call(Name, MFA)
	fusebox_app_sup:call(Name, MFA).
