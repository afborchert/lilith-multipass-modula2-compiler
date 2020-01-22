DEFINITION MODULE FilePool;         (* LG 09.06.80 *)

  FROM FileSystem IMPORT File;

  EXPORT QUALIFIED
    FilePointer, GetFileNumber, ReturnFileNumber,
    GetFileFromPool, ReturnFileToPool;

  TYPE FilePointer = POINTER TO File;

  PROCEDURE GetFileNumber(VAR filenum: CARDINAL; VAR error: BOOLEAN);
    (* Get a file number from pool. Error is set when pool is full *)

  PROCEDURE ReturnFileNumber(filenum: CARDINAL); 
    (* Return the file belonging to filenum to pool *)

  PROCEDURE GetFileFromPool(filenum: CARDINAL; VAR file: FilePointer;
                            VAR error: BOOLEAN);
    (* Return the file belonging to filenum *)
    (* Error is set when no file exists     *)

  PROCEDURE ReturnFileToPool(file: FilePointer); 
    (* Return the file to pool *) 
 
END FilePool.
