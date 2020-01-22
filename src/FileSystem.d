DEFINITION MODULE FileSystem; (*S.E.Knudsen for Lilith*)
 FROM SYSTEM IMPORT ADDRESS, WORD;
 EXPORT QUALIFIED
  File, Response,

  Create, Close, Lookup, Rename,
  SetRead, SetWrite, SetModify, SetOpen,
  Doio, SetPos, GetPos, Length,

  Reset, Again,
  ReadWord, WriteWord, ReadChar, WriteChar,

  Command, Flag, FlagSet;

TYPE
 Response = (done, notdone, notsupported, callerror,
        unknownmedium, unknownfile, paramerror,
        toomanyfiles, eom, deviceoff,
        softparityerror, softprotected, softerror,
        hardparityerror, hardprotected, timeout, harderror);
 Command  = (create, open, close, lookup, rename,
        setread, setwrite, setmodify, setopen,
        doio, setpos, getpos, length,
        setprotect, getprotect, setpermanent, getpermanent,
        getinternal);
Flag      = (er, ef, rd, wr, ag, bytemode);
FlagSet   = SET OF Flag;

File      = RECORD res: Response;
        bufa, ela, ina, topa: ADDRESS;
        elodd, inodd, eof: BOOLEAN;
        flags: FlagSet;
        CASE com: Command OF
         create, open, getinternal: fileno, versionno: CARDINAL |
         lookup: new: BOOLEAN |
         setpos, getpos, length: highpos, lowpos: CARDINAL |
         setprotect, getprotect: wrprotect: BOOLEAN |
         setpermanent, getpermanent: on: BOOLEAN
        END;
       END;

PROCEDURE Create(VAR f:File; mediumname: ARRAY OF CHAR);

PROCEDURE Close(VAR f:File);

PROCEDURE Lookup(VAR f:File; filename: ARRAY OF CHAR; new:BOOLEAN);

PROCEDURE Rename(VAR f:File; filename: ARRAY OF CHAR);

PROCEDURE SetRead(VAR f:File);

PROCEDURE SetWrite(VAR f:File);

PROCEDURE SetModify(VAR f:File);

PROCEDURE SetOpen(VAR f:File);

PROCEDURE Doio(VAR f:File);

PROCEDURE SetPos(VAR f:File; highpos, lowpos: CARDINAL);

PROCEDURE GetPos(VAR f:File; VAR highpos, lowpos: CARDINAL);

PROCEDURE Length(VAR f:File; VAR highpos, lowpos: CARDINAL);

PROCEDURE Reset(VAR f:File);

PROCEDURE Again(VAR f:File);

PROCEDURE ReadWord(VAR f:File; VAR w: WORD);

PROCEDURE WriteWord(VAR f:File; w: WORD);

PROCEDURE ReadChar(VAR f:File; VAR ch: CHAR);

PROCEDURE WriteChar(VAR f:File; ch: CHAR);

END FileSystem.
