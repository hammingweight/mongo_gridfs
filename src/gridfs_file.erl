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
%%% @doc MongoDB GridFS File API. This module provides functions for reading from
%%%      a GridFS file and getting file information about a file. 
%%% @end

-module(gridfs_file).

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

-record(state, {connection_parameters, bucket, id}).

%% External functions
new(ConnectionParameters, Bucket, Id) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters, Bucket, Id], []),
	Pid.

%% Server functions

%% @doc Initializes the server with connection parameters, a bucket and an ID.
init([ConnectionParameters, Bucket, Id]) ->
	State = #state{connection_parameters=ConnectionParameters, bucket=Bucket, id=Id},
    {ok, State}.


%% @doc Responds synchronously to server calls.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc Responds to asynchronous server calls.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Responds to out-of-band messages. The server ignores any such messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Handles the shutdown of the server.
terminate(_Reason, _State) ->
    ok.

%% @doc Responds to code changes. Any code changes are ignored (the server's state 
%%      is unchanged).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

