DEFINITION MODULE Terminal; (* A. Borchert *)
 EXPORT QUALIFIED
  Read, BusyRead, ReadAgain, Write, WriteLn, WriteString;

 PROCEDURE Read(VAR ch: CHAR);

 PROCEDURE BusyRead(VAR ch: CHAR);

 PROCEDURE ReadAgain;

 PROCEDURE Write(ch: CHAR);

 PROCEDURE WriteLn;

 PROCEDURE WriteString(s: ARRAY OF CHAR);

END Terminal.
