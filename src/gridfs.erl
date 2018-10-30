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
%%% @doc MongoDB GridFS API. This module provides functions for creating, reading, updating and 
%%       deleting files from GridFS. The exported functions exposed are similar to the CRUD 
%%       functions exposed by the mongo API of the MongoDB driver. 
%%% @end

-module(gridfs).

-behaviour(gen_server).

%% Includes
-include("gridfs.hrl").

%% Types
-type(action() :: fun()).

%% API
-export([delete/1,
		 delete/2,
		 delete_one/1,
		 delete_one/2,
		 do/5,
		 find_one/1,
		 find_one/2,
		 find/1,
		 find/2,
		 insert/2,
         insert_with_bson/2,
		 insert/3]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% External functions

%@doc Deletes files matching the selector from the fs.files and fs.chunks collections.
-spec(delete(bson:document()) -> ok).
delete(Selector) ->
	delete(fs, Selector).

%@doc Deletes files matching the selector from the specified bucket.
-spec(delete(atom(), bson:document()) -> ok).
delete(Bucket, Selector) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	ChunksColl = list_to_atom(atom_to_list(Bucket) ++ ".chunks"),
	Cursor = mongo:find(FilesColl, Selector, {'_id', 1}),
	Files = mongo_cursor:rest(Cursor),
	mongo_cursor:close(Cursor),
	Ids = [Id || {'_id', Id} <- Files],
	mongo:delete(ChunksColl, {files_id, {'$in', Ids}}),
	mongo:delete(FilesColl, {'_id', {'$in', Ids}}).

%@doc Deletes the first file matching the selector from the fs.files and fs.chunks collections.
-spec(delete_one(bson:document()) -> ok).
delete_one(Selector) ->
	delete_one(fs, Selector).

%@doc Deletes the first file matching the selector from the specified bucket.
-spec(delete_one(atom(), bson:document()) -> ok).
delete_one(Bucket, Selector) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	ChunksColl = list_to_atom(atom_to_list(Bucket) ++ ".chunks"),
	case mongo:find_one(FilesColl, Selector, {'_id', 1}) of
		{{'_id', Id}} ->
			mongo:delete(ChunksColl, {files_id, Id}),
			mongo:delete_one(FilesColl, {'_id', Id});
		{} ->
			ok
	end.

%% @doc Executes an 'action' using the specified read and write modes to a database using a connection.
%%      An 'action' is a function that takes no arguments. The fun will usually invoke functions
%%      to do inserts, finds, modifies, deletes, etc.
-spec(do(mongo:write_mode(), mongo:read_mode(), mongo:connection()|mongo:rs_connection(),mongo:db(), mongo:action()) -> {ok, any()}|{failure, any()}).
do(WriteMode, ReadMode, Connection, Database, Action) ->
	%% Since we need to store state information, we spawn a new process for this
	%% function so that if the Action also invokes the 'do' function we don't wind up trashing
	%% the original state.
	ConnectionParameters = #gridfs_connection{write_mode=WriteMode, read_mode=ReadMode, connection=Connection, database=Database},
	{ok, Pid} = gen_server:start_link(?MODULE, [ConnectionParameters], []),
	gen_server:call(Pid, {do, Action}, infinity).

%@doc Finds the first file matching the selector from the fs.files and fs.chunks collections.
-spec(find_one(bson:document()) -> file()).
find_one(Selector) ->
	find_one(fs, Selector).

%@doc Finds the first file matching the selector from the specified bucket.
-spec(find_one(atom(), bson:document()) -> file()).
find_one(Bucket, Selector) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	{{'_id', Id}} = mongo:find_one(FilesColl, Selector, {'_id', 1}),
	ConnectionParameters = get(gridfs_state),
	gridfs_file:new(ConnectionParameters, Bucket, Id, self()).

%@doc Finds files matching the selector from the fs.files and fs.chunks collections
%     and returns a cursor.
-spec(find(bson:document()) -> cursor()).
find(Selector) ->
	find(fs, Selector).

%@doc Finds files matching the selector from the specified bucket
%     and returns a cursor.
-spec(find(atom(), bson:document()) -> cursor()).
find(Bucket, Selector) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	MongoCursor = mongo:find(FilesColl, Selector, {'_id', 1}),
	ConnectionParameters = get(gridfs_state),
	gridfs_cursor:new(ConnectionParameters, Bucket, MongoCursor, self()).
	
%@doc Inserts a file with a specified name into the default bucket.
%     The file contents can be passed as either data or a file process opened for
%     reading.
insert(FileName, FileData) ->
	insert_with_bson({filename, FileName}, FileData).

%@doc Inserts a file with a specified bson document into the default bucket.
%     The file contents can be passed as either data or a file process opened for
%     reading.

insert_with_bson(BsonDocument, FileData) ->
    insert(fs, BsonDocument, FileData).

%@doc Inserts a file with a bson document or filename into the specified bucket.
%     The file contents can be passed as either data or a file process opened for
%     reading.

insert(Bucket, FileName, FileData) when not is_tuple(FileName) ->
    insert(Bucket, {filename, FileName}, FileData);
insert(Bucket, Bson, FileData) when is_binary(FileData) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	ChunksColl = list_to_atom(atom_to_list(Bucket) ++ ".chunks"),
	ObjectId = mongodb_app:gen_objectid(),
	insert(ChunksColl, ObjectId, 0, FileData),
	Md5 = list_to_binary(bin_to_hexstr(crypto:hash(md5,FileData))),
    ListBson=tuple_to_list(Bson),
    ListFileAttr=['_id', ObjectId, length, size(FileData), chunkSize, ?CHUNK_SIZE, uploadDate, erlang:timestamp(), md5, Md5],
    UnifiedList=lists:append([ListFileAttr, ListBson]),
	mongo:insert(FilesColl, list_to_tuple(UnifiedList));
insert(Bucket, Bson, IoStream) ->
	FilesColl = list_to_atom(atom_to_list(Bucket) ++ ".files"),
	ChunksColl = list_to_atom(atom_to_list(Bucket) ++ ".chunks"),
	ObjectId = mongodb_app:gen_objectid(),
	{Md5, FileSize} = copy(ChunksColl, ObjectId, 0, IoStream, crypto:hash_init(md5), 0),
	Md5Str = list_to_binary(bin_to_hexstr(Md5)),
	file:close(IoStream),
    ListBson=tuple_to_list(Bson),
    ListFileAttr=['_id', ObjectId, length, FileSize, chunkSize, ?CHUNK_SIZE, 
							 uploadDate, erlang:timestamp(), md5, Md5Str],
    UnifiedList=lists:append([ListFileAttr, ListBson]),
	mongo:insert(FilesColl, list_to_tuple(UnifiedList)).





%% Server functions

%% @doc Initializes the server with a write mode, read mode, a connection and database.
-spec(init([State::#gridfs_connection{}]) -> {ok, State::#gridfs_connection{}}).
init([State]) ->
    {ok, State}.

%% @doc Responds synchronously to server calls.  The action of the do/5 function is executed by
%%      this function. The process is stopped after this call.
-spec(handle_call({do, action()}, pid(), #gridfs_connection{}) -> {stop, normal, any(), #gridfs_connection{}}).
handle_call({do, Action}=_Request, _From, State) ->
    Reply = mongo:do(State#gridfs_connection.write_mode, State#gridfs_connection.read_mode, 
					 State#gridfs_connection.connection, State#gridfs_connection.database,
					 fun() ->
							 put(gridfs_state, State),
							 Action()
					 end),
    {stop, normal, Reply, State}.

%% @doc Responds asynchronously to messages. The server ignores any asynchronous messages.
-spec(handle_cast(any(), State::#gridfs_connection{}) -> {noreply, State::#gridfs_connection{}}).
handle_cast(_Message, State) ->
	{noreply, State}.

%% @doc Responds to out-of-band messages. The server ignores any such messages.
-spec(handle_info(any(), State::#gridfs_connection{}) -> {noreply, State::#gridfs_connection{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc Handles the shutdown of the server.
-spec(terminate(any(), #gridfs_connection{}) -> ok).
terminate(_Reason, _State) ->
	ok.

%% @doc Responds to code changes. Any code changes are ignored (the server's state is unchanged).
-spec(code_change(any(), State::#gridfs_connection{}, any()) -> {ok, State::#gridfs_connection{}}).
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%% Internal functions
bin_to_hexstr(Bin) ->
	lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

insert(Coll, ObjectId, N, Data) when size(Data) =< ?CHUNK_SIZE ->
	mongo:insert(Coll, {'files_id', ObjectId, data, {bin, bin, Data}, n, N});
insert(Coll, ObjectId, N, Data) ->
	<<Data1:(?CHUNK_SIZE*8), Data2/binary>> = Data,
	mongo:insert(Coll, {'files_id', ObjectId, data, {bin, bin, <<Data1:(?CHUNK_SIZE*8)>>}, n, N}),
	insert(Coll, ObjectId, N+1, Data2).

copy(ChunksColl, ObjectId, N, IoStream, Md5Context, Size) ->
	case file:pread(IoStream, N * ?CHUNK_SIZE, ?CHUNK_SIZE) of
		eof ->
			{crypto:hash_final(Md5Context), Size};
		{ok, Data} ->
			mongo:insert(ChunksColl, {'files_id', ObjectId, data, {bin, bin, Data}, n, N}),
			copy(ChunksColl, ObjectId, N+1, IoStream, crypto:hash_update(Md5Context, Data), Size+size(Data))
	end.
