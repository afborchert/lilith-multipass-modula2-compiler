DEFINITION MODULE NewStreams;        (* AG / LG 27.06.80 *)

  (* representation of module Streams with a modified *)
  (* definition module independent from a file system *)
 
  (* conventions for character streams :              *)
  (*    end of line is marked by one character (eoln) *)
  (*    end of stream is marked by a 0C character     *)

  FROM SYSTEM IMPORT WORD;
 
  EXPORT QUALIFIED 
    STREAM, eolc,
    Connect, Disconnect, Reset,
    WriteWord, WriteChar, EndWrite,
    ReadWord, ReadChar, EOS,
    GetPos, SetPos;

  TYPE STREAM;

  VAR eolc : CHAR; (* "end of line" character    *)
                   (* its value is less than 40C *)

  PROCEDURE Connect(VAR s: STREAM; filenum: CARDINAL; ws: BOOLEAN);
    (* connect a stream with a file referenced by filenum *)
    (* ws means: "s is a word, not a character stream *)

  PROCEDURE Disconnect(VAR s: STREAM; closefile: BOOLEAN);

  PROCEDURE Reset(s: STREAM);
    (* reset the stream s *)

  PROCEDURE WriteWord(s: STREAM; w: WORD);

  PROCEDURE WriteChar(s: STREAM; ch: CHAR);

  PROCEDURE EndWrite(s: STREAM);
    (* terminate writing on stream s *)

  PROCEDURE ReadWord(s: STREAM; VAR w: WORD);

  PROCEDURE ReadChar(s: STREAM; VAR ch: CHAR);

  PROCEDURE EOS(s: STREAM): BOOLEAN;
    (* end of stream test *)

  PROCEDURE GetPos(s: STREAM; VAR highpos, lowpos: CARDINAL);
    (* get current position on the stream s *)
    (* highpos and lowpos build a long integer *)

  PROCEDURE SetPos(s: STREAM; highpos, lowpos: CARDINAL);
    (* set stream s to indicated position *)
    (* highpos and lowpos build a long integer *)
    (* use this procedure with GetPos *)

END NewStreams.
