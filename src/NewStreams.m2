IMPLEMENTATION MODULE NewStreams;     (* LG 14.10.80 *)
  (* implementation for Personal Computer *)
 
  IMPORT FileSystem, FilePool;
  FROM SYSTEM IMPORT WORD, TSIZE;
  FROM FileSystem IMPORT Response, Flag, FlagSet;
  FROM Terminal IMPORT WriteString, WriteLn;

  CONST eofc = 0C;

  TYPE STREAM = FilePool.FilePointer;

  PROCEDURE Error(VAR s: STREAM);
  BEGIN
    WriteString(" ---- illegal operation on stream");
    WriteLn;
    HALT;
  END Error;

  PROCEDURE Reset(s: STREAM);
  BEGIN 
    FileSystem.SetOpen(s^);
    FileSystem.SetPos(s^, 0, 0)
  END Reset;

  PROCEDURE Connect(VAR s: STREAM; filenum: CARDINAL; ws: BOOLEAN);
    VAR err : BOOLEAN;
  BEGIN
    FilePool.GetFileFromPool(filenum,s,err);
    IF err THEN Error(s);
    ELSE Reset(s);
    END;
  END Connect;
  
  PROCEDURE Disconnect(VAR s: STREAM; closefile: BOOLEAN);
  BEGIN
    IF closefile THEN
      FileSystem.Close(s^);
      IF s^.res <> done THEN Error(s) END;
      FilePool.ReturnFileToPool(s);
    END; 
  END Disconnect;

  PROCEDURE WriteWord(s: STREAM; w: WORD);
  BEGIN
    WITH s^ DO
      LOOP 
        IF flags*FlagSet{wr, bytemode, er} <> FlagSet{wr} THEN 
          IF NOT (wr IN flags) THEN
            IF rd IN flags THEN
              Error(s); 
            ELSE
              FileSystem.SetWrite(s^);
              IF elodd THEN Error(s) END;
            END;
          END;
          IF er IN flags THEN RETURN END; 
          IF bytemode IN flags THEN
            Error(s);
          END;
        ELSIF ela >= topa THEN FileSystem.Doio(s^)  
        ELSE ela^ := w; INC(ela, TSIZE(WORD)); RETURN
        END 
      END (* loop *)   
    END
  END WriteWord;

  PROCEDURE WriteChar(s: STREAM; ch: CHAR);
  BEGIN
    WITH s^ DO
      LOOP
        IF flags*FlagSet{wr, bytemode, er} <> FlagSet{wr, bytemode} THEN
          IF NOT (wr IN flags) THEN
            IF rd IN flags THEN
              Error(s);
            ELSE
              FileSystem.SetWrite(s^); INCL(flags, bytemode);
            END;
          END;
          IF er IN flags THEN RETURN END;
          IF NOT (bytemode IN flags) THEN
            Error(s); 
          END; 
        ELSIF ela >= topa THEN FileSystem.Doio(s^)
        ELSIF elodd THEN
          ela^ := WORD(CARDINAL(ela^) DIV 400B * 400B + CARDINAL(ch) MOD 400B);
          INC(ela, TSIZE(WORD)); elodd := FALSE;
          IF ela >= topa THEN FileSystem.Doio(s^) END;
          RETURN
        ELSE
          ela^ := WORD(CARDINAL(ela^) MOD 400B + CARDINAL(ch) MOD 400B * 400B);
          elodd := TRUE;
          RETURN
        END
      END
    END
  END WriteChar;

  PROCEDURE EndWrite(s: STREAM);
  BEGIN
    FileSystem.SetOpen(s^);
    IF s^.res <> done THEN Error(s) END;
  END EndWrite;

  PROCEDURE ReadWord(s: STREAM; VAR w: WORD);
  BEGIN
    WITH s^ DO    
      LOOP 
        IF flags*FlagSet{rd, bytemode, ef} <> FlagSet{rd} THEN  
          IF NOT (rd IN flags) THEN
            IF wr IN flags THEN
              Error(s);
            ELSE
              FileSystem.SetRead(s^);  
              IF elodd OR inodd THEN Error(s) END;
            END
          END;
          IF eof THEN RETURN END;
          IF bytemode IN flags THEN 
            Error(s);
          END;
        ELSIF ela >= ina THEN FileSystem.Doio(s^)
        ELSE w := ela^; INC(ela, TSIZE(WORD)); RETURN
        END
      END (* loop *)  
    END
  END ReadWord;

  PROCEDURE ReadChar(s: STREAM; VAR ch: CHAR);
  BEGIN
    WITH s^ DO
      LOOP
        IF flags*FlagSet{rd, bytemode, ef} <> FlagSet{rd, bytemode} THEN
          IF NOT (rd IN flags) THEN 
            IF wr IN flags THEN
              Error(s);
            ELSE
              FileSystem.SetRead(s^); INCL(flags, bytemode);
            END;
          END;
          IF eof THEN ch := eofc; RETURN END;
          IF NOT (bytemode IN flags) THEN
            Error(s);
          END;
        ELSIF (ela>=ina) AND ((elodd>=inodd) OR (ela>ina)) THEN FileSystem.Doio(s^)
        ELSIF elodd THEN
          ch := CHAR(CARDINAL(ela^) MOD 400B);
          INC(ela, TSIZE(WORD)); elodd := FALSE;
          RETURN
        ELSE
          ch := CHAR(CARDINAL(ela^) DIV 400B);
          elodd := TRUE;
          RETURN
        END;
      END; (* loop *)  
    END; 
  END ReadChar;

  PROCEDURE EOS(s: STREAM): BOOLEAN;
  BEGIN
    RETURN s^.eof;
  END EOS;

  PROCEDURE GetPos(s: STREAM; VAR highpos, lowpos: CARDINAL);
  BEGIN
    FileSystem.GetPos(s^,highpos,lowpos);
  END GetPos;

  PROCEDURE SetPos(s: STREAM; highpos, lowpos: CARDINAL);
  BEGIN
    FileSystem.SetPos(s^,highpos,lowpos);
  END SetPos;

BEGIN (* NewStreams *)
  eolc := 12C;
END NewStreams.
