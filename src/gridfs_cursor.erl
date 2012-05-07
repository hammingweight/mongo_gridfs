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
-export([close/1,
		 new/4,
		 next/1,
		 rest/1,
		 set_timeout/2,
		 take/2]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {connection_parameters, bucket, mongo_cursor, parent_process, die_with_parent=true, timeout=infinity}).

%% External functions
close(Pid) ->
	gen_server:call(Pid, close, infinity).
	
new(ConnectionParameters, Bucket, MongoCursor, ParentProcess) ->
	{ok, Cursor} = gen_server:start_link(?MODULE, [ConnectionParameters, Bucket, MongoCursor, ParentProcess], []),
	Cursor.

next(Cursor) ->
	gen_server:call(Cursor, next, infinity).

rest(Cursor) ->
	gen_server:call(Cursor, rest, infinity).

set_timeout(Cursor, Timeout) ->
	gen_server:call(Cursor, {set_timeout, Timeout}, infinity).
	
take(Limit, Cursor) when Limit >= 0 ->
	gen_server:call(Cursor, {take, Limit}, infinity).

%% Server functions

%% @doc Initializes the server with connection parameters, a bucket and a mongo cursor.
init([ConnectionParameters, Bucket, MongoCursor, ParentProcess]) ->
	monitor(process, ParentProcess),
    {ok, #state{connection_parameters=ConnectionParameters, 
				bucket=Bucket, 
				mongo_cursor=MongoCursor, 
				parent_process=ParentProcess}}.

%% @doc Responds synchronously to messages.
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call(next, _From, State) ->
	MongoCursor = State#state.mongo_cursor,
	case mongo_cursor:next(MongoCursor) of
		{} ->
			{stop, normal, {}, State};
		{{'_id', Id}} ->
			{reply, create_file(State, Id), State, State#state.timeout}
	end;
handle_call(rest, _From, State) ->
	MongoCursor = State#state.mongo_cursor,
	Ids = mongo_cursor:rest(MongoCursor),
	Reply = [create_file(State, Id) || {'_id', Id} <- Ids],
	{stop, normal, Reply, State};
handle_call({set_timeout, Timeout}, _From, State) ->
	{reply, ok, State#state{die_with_parent=false, timeout=Timeout}, Timeout};
handle_call({take, Limit}, _From, State) ->
	Files = take(State, Limit, []),
	{stop, normal, Files, State}.

%% @doc Handles asynchronous messages.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.timeout}.

%% @doc Handles out-of-band messages.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) when Pid =:= State#state.parent_process andalso State#state.die_with_parent ->
	{stop, normal, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State, State#state.timeout}.

%% @doc Shuts down the server.
terminate(_Reason, _State) ->
    ok.

%% @doc Handles code changes (changes are ignored).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
create_file(State, Id) ->
	ConnectionParameters = State#state.connection_parameters,
	ParentProcess = State#state.parent_process,
	Bucket = State#state.bucket,
	File = gridfs_file:new(ConnectionParameters, Bucket, Id, ParentProcess),
	case State#state.die_with_parent of
		true ->
			File;
		false ->
			gridfs_file:set_timeout(File, State#state.timeout),
			File
	end.

% Reads files from a cursor up to a limit and returns them as a list.
take(_State, 0, Files) ->
	lists:reverse(Files);
take(State, Limit, Files) ->
	MongoCursor = State#state.mongo_cursor,
	case mongo_cursor:next(MongoCursor) of
		{} ->
			lists:reverse(Files);
		{{'_id', Id}} ->
			File = create_file(State, Id),
			take(State, Limit-1, [File|Files])
	end.
