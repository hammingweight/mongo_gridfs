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

	> application:start(mongodb)
	
**mongo_gridfs** is a library application that does not need to be started.

#### Example code
These examples assume that we have a file, "file1.txt", that can be read. The code below starts the MongoDB driver, creates 
the file and gets a connection to the database that we'll use in subsequent code.

	> 1> application:start(mongodb).
	  ok
	  2> file:write_file(<<"file1.txt">>, <<"Some sample text.\n">>).
	  ok
	  3> {ok,Conn}=mongo:connect(localhost).
	  {ok,{connection,{"localhost",27017},<0.40.0>,infinity}}


##### Creating (Inserting) Files
To insert a file into GridFS, you can pass either the contents of the file (a binary) or the process identifier of an opened file.
The code below illustrates both methods.
	