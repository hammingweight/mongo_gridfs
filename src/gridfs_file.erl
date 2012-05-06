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
		 new/4,
		 md5/1,
		 pread/3,
		 read_file/1,
		 set_timeout/2]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(state, {connection_parameters, bucket, id, parent_process, die_with_parent=true, timeout=infinity}).

%% External functions
close(Pid) ->
	gen_server:call(Pid, close, infinity).

file_name(Pid) ->
	gen_server:call(Pid, file_name, infinity).
	
file_size(Pid) ->
	gen_server:call(Pid, file_size, infinity).
	
md5(Pid) ->
	gen_server:call(Pid, md5, infinity).
	
new(ConnectionParameters, Bucket, Id, ParentProcess) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters, Bucket, Id, ParentProcess], []),
	Pid.

pread(Pid, Offset, Length) ->
	gen_server:call(Pid, {pread, Offset, Length}, infinity).

read_file(Pid) ->
	Data = gen_server:call(Pid, read_file, infinity),
	Data.
	
set_timeout(Pid, Timeout) ->
	gen_server:call(Pid, {set_timeout, Timeout}, infinity).
	

%% Server functions

%% @doc Initializes the server with connection parameters, a bucket and an ID.
init([ConnectionParameters, Bucket, Id, ParentProcess]) ->
	monitor(process, ParentProcess),
	State = #state{connection_parameters=ConnectionParameters, bucket=Bucket, id=Id, parent_process=ParentProcess},
    {ok, State}.


%% @doc Responds synchronously to server calls.
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call(file_size, _From, State) ->
    Length = get_attribute(State, length),
    {reply, {ok, Length}, State, State#state.timeout};
handle_call(md5, _From, State) ->
    Md5 = get_attribute(State, md5),
    {reply, {ok, Md5}, State, State#state.timeout};
handle_call(file_name, _From, State) ->
    FileName = get_attribute(State, filename),
    {reply, {ok, FileName}, State, State#state.timeout};
handle_call(read_file, _From, State) ->
	ChunkSize = get_attribute(State, chunkSize),
	Length = get_attribute(State, length),
	NumChunks = (Length + ChunkSize - 1) div ChunkSize, 
	Reply = read(State, 0, 0, Length, NumChunks, <<>>),
	{reply, {ok, Reply}, State, State#state.timeout};
handle_call({pread, Offset, NumToRead}, _From, State) ->
	ChunkSize = get_attribute(State, chunkSize),
	Length = get_attribute(State, length),
	case Offset >= Length of
		false ->
			NumChunks = (Length + ChunkSize - 1) div ChunkSize,
			ChunkNum = Offset div ChunkSize,
			ChunkOffset = Offset rem ChunkSize,
			Reply = read(State, ChunkNum, ChunkOffset, NumToRead, NumChunks, <<>>),
			{reply, {ok, Reply}, State, State#state.timeout};
		true ->
			{reply, eof, State, State#state.timeout}
	end;
handle_call({set_timeout, Timeout}, _From, State) ->
	{reply, ok, State#state{die_with_parent=false, timeout=Timeout}, Timeout}.
			

%% @doc Responds to asynchronous server calls.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.timeout}.

%% @doc Responds to out-of-band messages. The server ignores any such messages.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) when Pid =:= State#state.parent_process andalso State#state.die_with_parent ->
	{stop, normal, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State, State#state.timeout}.

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

read(_State, ChunkNum, _Offset, _NumToRead, NumberOfChunks, Result) when ChunkNum >= NumberOfChunks ->
	Result;
read(_State, _ChunkNum, _Offset, NumToRead, _NumberOfChunks, Result) when NumToRead =< 0 ->
	Result;
read(State, ChunkNum, Offset, NumToRead, NumberOfChunks, Result) ->
	Coll = list_to_atom(atom_to_list(State#state.bucket) ++ ".chunks"),
	Parameters = State#state.connection_parameters,
	WriteMode = Parameters#gridfs_connection.write_mode,
	ReadMode = Parameters#gridfs_connection.read_mode,
	Conn = Parameters#gridfs_connection.connection,
	Database = Parameters#gridfs_connection.database,
	{ok, {{data,{bin, bin, BinData}}}} = mongo:do(WriteMode, ReadMode, Conn, Database,
												  fun() ->
														  mongo:find_one(Coll, {'files_id', State#state.id, n, ChunkNum}, {'_id', 0, data, 1})
												  end),
	if
		Offset > 0 ->
			ListData1 = binary_to_list(BinData),
			{_, ListData2} = lists:split(Offset, ListData1),
			BinData2 = list_to_binary(ListData2);
		true ->
			BinData2 = BinData
	end,
	if
		size(BinData2) > NumToRead ->
			ListData3 = binary_to_list(BinData2),
			{ListData4, _} = lists:split(NumToRead, ListData3),
			BinData3 = list_to_binary(ListData4),
			<<Result/binary, BinData3/binary>>;
		true ->
			read(State, ChunkNum+1, 0, NumToRead-size(BinData2), NumberOfChunks, <<Result/binary, BinData2/binary>>)
	end.
