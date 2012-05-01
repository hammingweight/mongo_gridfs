% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%%% @author CA Meijer
%%% @copyright 2012 CA Meijer
%%% @doc MongoDB GridFS Cursor API. This module provides functions for retrieving GridFS
%%%      files from a cursor. 
%%% @end

-module(gridfs_cursor).

-behaviour(gen_server).

%% Includes
-include("gridfs.hrl").

%% API
-export([new/3]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {connection_parameters, bucket, mongo_cursor}).

%% External functions
new(ConnectionParameters, Bucket, MongoCursor) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters, Bucket, MongoCursor], []),
	Pid.

%% Server functions

%% @doc Initializes the server with connection parameters, a bucket and a mongo cursor.
init([ConnectionParameters, Bucket, MongoCursor]) ->
    {ok, #state{connection_parameters=ConnectionParameters, bucket=Bucket, mongo_cursor=MongoCursor}}.

%% @doc Responds synchronously to messages.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handles asynchronous messages.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handles out-of-band messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Shuts down the server.
terminate(_Reason, _State) ->
    ok.

%% @doc Handles code changes (changes are ignored).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

