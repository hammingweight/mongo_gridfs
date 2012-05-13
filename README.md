mongo_gridfs
============

This Erlang library provides access to files stored in a MongoDB database using the GridFS convention.

### Installing
The easiest way to install **mongo_gridfs** is by downloading the [archive](https://github.com/hammingweight/mongo_gridfs/downloads)
and unzipping it into a directory referenced in the $ERL_LIBS environment variable. **mongo_gridfs** uses the (official) 10gen 
Erlang driver so will you have to have installed that before using this library.

### API Documentation
The API is documented in the **docs** directory of the archive. The API is similar to the MongoDB driver except that files rather
than documents are passed as arguments to functions. 

### Running mongo_gridfs
To run mongo_gridfs, you must first start the **mongodb** application

	application:start(mongodb)
	
**mongo_gridfs** is a library application that does not need to be started.

#### Example code
These examples assume that we have a file, "file1.txt", that can be read. The code below starts the MongoDB driver, creates 
the file and gets a connection to the database that we'll use in subsequent code.

	1> application:start(mongodb).
	ok
	2> file:write_file(<<"file1.txt">>, <<"Some sample text.\n">>).
	ok
	3> {ok,Conn}=mongo:connect(localhost).
	{ok,{connection,{"localhost",27017},<0.40.0>,infinity}}


##### Creating (Inserting) Files
To insert a file into GridFS, you can pass either the contents of the file (a binary) or the process identifier of an opened file.
The code below shows the first method (notice how we use the `gridfs:do/5` function instead of the similar `mongo:do/5` function)

	4> gridfs:do(safe,master,Conn,mongo_gridfs,fun()->
	4> gridfs:insert(<<"file2.txt">>, <<"A GridFS file.\n">>)
	4> end).
	{ok,{<<79,175,136,5,138,250,150,11,58,0,0,1>>}}
	5> 

The code below reads from the filesystem and inserts the file contents into GridFS

	5> {ok,File}=file:open(<<"file1.txt">>, [read, binary]).
	{ok,<0.48.0>}
	6> gridfs:do(safe,master,Conn,mongo_gridfs,fun()->       
	6> gridfs:insert(<<"file1.txt">>, File)                  
	6> end).
	{ok,{<<79,175,139,2,138,250,150,11,133,0,0,3>>}}
	7>
	
##### Reading (Finding) Files
**mongo_gridfs** has `find` and `find_one` functions for reading files. The `find_one` function returns a file process,
the contents of the file can be read using `read_file/1` and `pread/3` functions. Metadata about the files is also
available using the `file_name/1`, `file_size/1` and `md5/1` functions). The `find` functions return a cursor; querying the cursor
returns access to an underlying file.

An important point about the file and cursor processes is that, by default, they terminate once the `gridfs:do/5` function that 
creates the processes completes. If you want to access the file or cursor from outside the scope of the `do/5` function, you
must set a timeout on the process. The example code below gets a file and sets a timeout of one minute.

	17> {ok,FilePid}=gridfs:do(safe,master,Conn,mongo_gridfs,fun()->
	17> Pid=gridfs:find_one({filename, <<"file1.txt">>}),           
	17> gridfs_file:set_timeout(Pid,60000),                         
	17> Pid                                                         
	17> end).                                                       
	{ok,<0.76.0>}
	18> gridfs_file:read_file(FilePid).                             
	{ok,<<"Some sample text.\n">>}
	19> 

##### Deleting Files
Deleting files can be done with the `delete` and `delete_one` functions.
