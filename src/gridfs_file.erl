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
-export([close/1,
		 file_name/1,
		 file_size/1,
		 new/3,
		 md5/1]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {connection_parameters, bucket, id}).

%% External functions
close(Pid) ->
	gen_server:call(Pid, close, infinity).

file_name(Pid) ->
	gen_server:call(Pid, file_name, infinity).
	
file_size(Pid) ->
	gen_server:call(Pid, file_size, infinity).
	
md5(Pid) ->
	gen_server:call(Pid, md5, infinity).
	
new(ConnectionParameters, Bucket, Id) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters, Bucket, Id], []),
	Pid.

%% Server functions

%% @doc Initializes the server with connection parameters, a bucket and an ID.
init([ConnectionParameters, Bucket, Id]) ->
	State = #state{connection_parameters=ConnectionParameters, bucket=Bucket, id=Id},
    {ok, State}.


%% @doc Responds synchronously to server calls.
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call(file_size, _From, State) ->
    Length = get_attribute(State, length),
    {reply, {ok, Length}, State};
handle_call(md5, _From, State) ->
    Md5 = get_attribute(State, md5),
    {reply, {ok, Md5}, State};
handle_call(file_name, _From, State) ->
    FileName = get_attribute(State, filename),
    {reply, {ok, FileName}, State}.
																		

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

get_attribute(State, Attribute) ->
	Coll = list_to_atom(atom_to_list(State#state.bucket) ++ ".files"),
	Parameters = State#state.connection_parameters,
	WriteMode = Parameters#gridfs_connection.write_mode,
	ReadMode = Parameters#gridfs_connection.read_mode,
	Conn = Parameters#gridfs_connection.connection,
	Database = Parameters#gridfs_connection.database,
	{ok, {{Attribute, Value}}} = mongo:do(WriteMode, ReadMode, Conn, Database,
										  fun() ->
												  mongo:find_one(Coll, {'_id', State#state.id}, {'_id', 0, Attribute, 1})
										  end),
	Value.
