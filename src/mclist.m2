(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCListing:                        *
*                                       * 
*     Listing generation                *
*                                       * 
*     Version C18 of 06.03.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

MODULE MCListing;    (* HHN / LG *)
 
  IMPORT MCBase, MCPublic, WriteStrings, Conversions;
  FROM NewStreams IMPORT
    STREAM, eolc, Connect, Reset, ReadWord, ReadChar,
    WriteChar, EndWrite, EOS, Disconnect;

  (* Special characters *)
  CONST
    Null = 0C;

  VAR
    Control2File : CARDINAL;
    Control3File : CARDINAL;
    Control4File : CARDINAL;
    SourceFile : CARDINAL;
    ListingFile : CARDINAL;
    Control : STREAM;
    Source : STREAM;
    Listing: STREAM;
 
  (* Error numbers *)
  CONST
    MaxErrNr = 1000; (* maximum number of errors *)
    NMaxErrNr = 4; (* number of char necessary to print an error number *)

  MODULE FailureMessages;

    FROM WriteStrings IMPORT WriteString, WriteLn;

    EXPORT FailMessType, FailMessSet, WriteFailMess;

    TYPE
      FailMessType = (EOFonControl,IncorrLineNr, ErrFirstOnControl,
                      NContrSmall);
      FailMessSet = SET OF FailMessType;

    VAR
      FailMessInhibit: FailMessSet;

    PROCEDURE WriteFailMess(fMess: FailMessType);
    BEGIN
      IF NOT (fMess IN FailMessInhibit) THEN
        CASE fMess OF
          EOFonControl:
            WriteString(" ---- EOF on Control");
         |IncorrLineNr:
            WriteString(" ---- Incorrect line number on Control");
         |ErrFirstOnControl:
            WriteString(" ---- Error message 1st element on Control");
         |NContrSmall:
            WriteString(" ---- NContr too small");
        END;
        WriteLn;
        (*INCL(FailMessInhibit,fMess);*)
      END;
    END WriteFailMess;
  
  BEGIN
    FailMessInhibit := FailMessSet{};
  END FailureMessages;
  
 MODULE ControlSystem;

  FROM MCBase IMPORT Symbol;
  FROM MCPublic IMPORT
    Compilerstatus, compstat, il1File, il2File;
  IMPORT
    Control, Control2File, Control3File, Control4File,
    EOS, Connect, Reset, ReadWord,
    WriteFailMess, FailMessType,
    MCBase;

  EXPORT Contr,ReadContr,PassBefore;

  MODULE InterfaceToPass2and3;
  
   FROM MCBase IMPORT Symbol;

   EXPORT Skip;

   VAR Skip: ARRAY Symbol OF CARDINAL;
       s: Symbol;

   BEGIN
    FOR s := eop TO anycon DO Skip[s] := 0 END;
    Skip[proceduresy] := 1;
    Skip[modulesy] := 1;
    Skip[ident] := 1;
    Skip[intcon] := 1;
    Skip[cardcon] := 1;
    Skip[intcarcon] := 1;
    Skip[realcon] := 2;
    Skip[charcon] := 1;
    Skip[stringcon] := 2;
    Skip[option] := 2;
    Skip[namesy] := 1;
    Skip[field] := 1;
    Skip[anycon] := 2;
  END InterfaceToPass2and3;

  CONST NContr = 256;
  TYPE ContrType = RECORD
                    CASE Eof: BOOLEAN OF
                     FALSE: CASE IsLineHead: BOOLEAN OF
                             FALSE: ErrNr, ErrPos: CARDINAL;
                            |TRUE: LineNr, Addr: CARDINAL;
                            END;
                    END;
                   END;
      
  VAR
    ContrBuff: ARRAY [0..NContr] OF ContrType;
    CurrElement, (* element read by GetNext *)
    Contr: ContrType; (* element provided by ReadContr ( =ContrBuff[Out] ) *)
    PassBefore : CARDINAL; (* number of compiler pass before lister *)

  PROCEDURE GetNext;
    (* read next element of file Control and put it into CurrElement *)
    VAR FirstCard : CARDINAL; (* first word of a record (as CARDINAL) *)
        FirstInt : INTEGER; (* first word of a record (as INTEGER) *)
        CurrSymbol : Symbol;
        dummy, i : CARDINAL;

    PROCEDURE TestControlEOS;
    BEGIN
      IF EOS(Control) THEN                                                        
        WriteFailMess(EOFonControl);
        HALT;
      END;
    END TestControlEOS; 

  BEGIN
    WITH CurrElement DO
      Eof := FALSE;         
      IF PassBefore <= 3 THEN (* after Pass 2 or Pass 3 *)
        LOOP (* until CurrSymbol IN {errorsy,eol,eop} *)
          ReadWord(Control,FirstCard);
          TestControlEOS;
          CurrSymbol := VAL(Symbol,FirstCard DIV 400B);
          IF CurrSymbol = eol THEN
            IsLineHead := TRUE;
            Addr := 0;
            ReadWord(Control,LineNr);
            TestControlEOS;
            EXIT;
          ELSIF CurrSymbol = errorsy THEN
            IsLineHead := FALSE;
            ErrPos := FirstCard MOD 400B;
            IF ErrPos = 0 THEN ErrPos := 1 END;
            ReadWord(Control,ErrNr);
            TestControlEOS;
            EXIT;
          ELSIF CurrSymbol = eop THEN 
            Eof := TRUE;
            EXIT;
          ELSE (* skip th whole symbol *)
            FOR i := 1 TO Skip[CurrSymbol] DO 
              ReadWord(Control,dummy);
              TestControlEOS;
            END; 
          END; 
        END; (* LOOP *) 
      ELSE (* after Pass 4 *)   
        ReadWord(Control,FirstInt);
        IF EOS(Control) THEN Eof := TRUE;
        ELSE
          IF FirstInt > 0 THEN (* line head *)
            IsLineHead := TRUE;
            LineNr := FirstInt;
            ReadWord(Control,Addr);
          ELSE (* error message *)
            IsLineHead := FALSE;           
            ErrNr := - FirstInt; 
            ReadWord(Control,ErrPos);
            IF ErrPos = 0 THEN ErrPos := 1 END;
          END;
          TestControlEOS;
        END;
      END;
    END; (* WITH *)
    Insert;
  END GetNext;

  VAR
   Inp,Out,Count,LastInsertion,LastLineNr: CARDINAL;

  PROCEDURE Insert;
   VAR This,Next,i,j: CARDINAL;
    RunOff: BOOLEAN;
    SaveLineNr: CARDINAL;
   BEGIN
    IF CurrElement.Eof THEN This := Inp;
    ELSE
     IF CurrElement.IsLineHead THEN
      This := Inp; Next := Inp; RunOff := Count=0;
      IF Next # Out THEN
       LOOP Next := (Next+NContr-1) MOD NContr;
        IF ContrBuff[Next].IsLineHead THEN EXIT;
        ELSIF Next = Out THEN WriteFailMess(NContrSmall); HALT;
        END;
       END;
      END;
      WHILE NOT RunOff AND (ContrBuff[Next].LineNr>CurrElement.LineNr) DO
       This := Next; SaveLineNr := ContrBuff[This].LineNr;
       IF Next # Out THEN
        LOOP Next := (Next+NContr-1) MOD NContr;
         WITH ContrBuff[Next] DO
          IF IsLineHead AND
            (LineNr<SaveLineNr) THEN EXIT;
          ELSIF Next = Out THEN
           IF NOT IsLineHead THEN
            WriteFailMess(NContrSmall); HALT;
           END;
           RunOff := TRUE; EXIT;
          END;
         END;
        END;
       ELSE RunOff := TRUE;
       END;
      END;
      IF RunOff THEN This := Next;
      ELSE
       IF ContrBuff[Next].LineNr=CurrElement.LineNr THEN This := Next END;
       Next := This;
       IF Next # Out THEN
        LOOP Next := (Next+NContr-1) MOD NContr;
         WITH ContrBuff[Next] DO
          IF NOT IsLineHead THEN EXIT;
          ELSIF LineNr<CurrElement.LineNr THEN EXIT;
          ELSIF Addr<=CurrElement.Addr THEN EXIT;
          ELSIF Next = Out THEN This := Next; EXIT;
          END;
          This := Next;
         END;
        END;
       END;
      END;
      LastLineNr := CurrElement.LineNr;
     ELSE (* error message *)
      IF ContrBuff[LastInsertion].IsLineHead OR
        (ContrBuff[LastInsertion].ErrPos<=CurrElement.ErrPos) THEN
       This := LastInsertion;
       LOOP This := (This+1) MOD NContr;
        WITH ContrBuff[This] DO
         IF This = Inp THEN EXIT;
         ELSIF IsLineHead THEN
          IF LineNr>LastLineNr THEN EXIT END;
         ELSE
          IF ErrPos>=CurrElement.ErrPos THEN EXIT END;
         END;
        END;
       END;
      ELSE
       IF LastInsertion = Out THEN WriteFailMess(NContrSmall); HALT END;
       This := LastInsertion; Next := This;
       LOOP Next := (Next+NContr-1) MOD NContr;
        WITH ContrBuff[Next] DO
         IF IsLineHead THEN EXIT;
         ELSIF ErrPos <= CurrElement.ErrPos THEN EXIT;
         ELSIF Next = Out THEN WriteFailMess(NContrSmall); HALT;
         END;
         This := Next;
        END;
       END;
      END;
     END;
     i := Inp; j := (Inp+NContr-1) MOD NContr;
     WHILE i # This DO
      ContrBuff[i] := ContrBuff[j];
      i := j; j := (j+NContr-1) MOD NContr;
     END;
    END;
    ContrBuff[This] := CurrElement; INC(Count);
    LastInsertion := This;
    Inp := (Inp+1) MOD NContr;
   END Insert;

  PROCEDURE ReadContr;
   BEGIN
    IF (Count = NContr) AND (Out = LastInsertion) THEN
     WriteFailMess(NContrSmall); HALT;
    END;
    Contr := ContrBuff[Out]; Out := (Out+1) MOD NContr;
    DEC(Count);
    IF NOT CurrElement.Eof THEN GetNext END;
   END ReadContr;

  BEGIN
    Control2File := il2File;
    Control3File := il1File;
    Control4File := il2File;
    Inp := 0; Out := 0; Count := 0;
    IF defs IN compstat THEN PassBefore := 2;
    ELSIF finis IN compstat THEN PassBefore := 4;
    ELSE PassBefore := 3;
    END;
    CASE PassBefore OF
      2: Connect(Control,Control2File,(*WordFlag=*)TRUE);
     |3: Connect(Control,Control3File,(*WordFlag=*)TRUE);
     |4: Connect(Control,Control4File,(*WordFlag=*)TRUE);
    END;
    Reset(Control);
    REPEAT GetNext UNTIL (Count=NContr) OR CurrElement.Eof;
  END ControlSystem;
      
 VAR
  LastErrNr, (* number of last error on same line *)
  LastErrPos: CARDINAL; (* position of last error on same line *)
 
 MODULE SourceAndListing;
 
  FROM Conversions IMPORT ConvertOctal, ConvertCardinal;
  FROM MCPublic IMPORT modFile, lstFile;
  IMPORT
    Null, Source, Listing, SourceFile, ListingFile,
    eolc, Connect, Reset, EOS, ReadChar, WriteChar,
    FailMessType, WriteFailMess, PassBefore,
    LastErrPos;
 
  EXPORT WriteEndLine,WriteNormHead,ReadWriteLine,
   WriteErrHead,WriteErrFlag,WriteErrNr; 
 
  VAR
   CurrPos: CARDINAL; (* current (next free) position on a line *)

  CONST
   NLineNr = 6; (* number of characters for line number *)
   NAddr = 7; (* number of characters for address *)
   NSpace = 2; (* number of characters for space before programme text *)

  VAR
   NHead: CARDINAL; (* NLineNr + NAddr + NSpace *)
   ErrHead: ARRAY [0 .. NLineNr+NAddr+NSpace] OF CHAR;
            (* header text of an error line *)
   SourceCh : CHAR;

  CONST
   Stars = " *****"; (* header of an error line *)
   NErr = 6; (* maximal number of characters for error numbers *)

  PROCEDURE WriteEndLine;
  BEGIN
    WriteChar(Listing,eolc);
  END WriteEndLine;
   
  PROCEDURE WriteNormHead(fLineNr,fAddr: CARDINAL);
   (* Write out a header of a normal listing line *)
    
   PROCEDURE WriteLineNr(fN: CARDINAL);
    (* Write out a line number in NLineNr characters with leading blanks *)
    VAR i: CARDINAL;
     Buff: ARRAY [1..NLineNr] OF CHAR; 
   BEGIN
     ConvertCardinal(fN,NLineNr,Buff);
     FOR i := 1 TO NLineNr DO WriteChar(Listing,Buff[i]) END;
   END WriteLineNr;
  
   PROCEDURE WriteAddr(fN: CARDINAL);
    (* Write out an address in NAddr characters with one leading blank *)
    VAR i: CARDINAL;
     Buff: ARRAY [1..NAddr] OF CHAR;
    BEGIN
     ConvertOctal(fN,NAddr,Buff);
     FOR i := 1 TO NAddr DO WriteChar(Listing,Buff[i]) END;
    END WriteAddr;
  
   PROCEDURE WriteSpace;
    (* Write out NSpace blanks *)
    VAR i: CARDINAL;
    BEGIN
     FOR i := 1 TO NSpace DO WriteChar(Listing,' ') END;
    END WriteSpace;
  
   BEGIN (* WriteNormHead *)
    WriteLineNr(fLineNr);
    IF PassBefore = 4 THEN WriteAddr(fAddr) END;
    WriteSpace;
    CurrPos := 1;
   END WriteNormHead;
   
  PROCEDURE WriteErrHead;
   (* Write out the header of an error line *)
   VAR i: CARDINAL;
   BEGIN 
    i := 0;
    WHILE (i<NHead) AND (ErrHead[i]#Null) DO
     WriteChar(Listing,ErrHead[i]); INC(i);
    END;
    FOR i := i+1 TO NHead DO WriteChar(Listing,' ') END;
    CurrPos := 1;
   END WriteErrHead;

  PROCEDURE WriteErrNr(fErrNr: CARDINAL);
   (* Write out an error number with a leading blank *)
   VAR
     Buff: ARRAY [1..NErr] OF CHAR; 
     ix : CARDINAL;
   BEGIN
    WriteChar(Listing,' '); INC(CurrPos);
    ConvertCardinal(fErrNr,1,Buff);
    ix := 1;
    WHILE (ix <= NErr) AND (Buff[ix] <> 0C) DO
      WriteChar(Listing,Buff[ix]);
      INC(ix);
      INC(CurrPos);
    END;
   END WriteErrNr;
  
  PROCEDURE WriteErrFlag(fErrPos: CARDINAL);
   BEGIN
    IF fErrPos = LastErrPos THEN WriteChar(Listing,','); INC(CurrPos);
    ELSE
     IF CurrPos > fErrPos THEN
       WriteEndLine;
       WriteErrHead;
     END;
     WHILE CurrPos < fErrPos DO
      WriteChar(Listing,' '); INC(CurrPos);
     END;
     WriteChar(Listing,'^'); INC(CurrPos);
    END;
   END WriteErrFlag;
  
  PROCEDURE ReadWriteLine;
  BEGIN
    IF NOT EOS(Source) THEN
      LOOP
        IF SourceCh = eolc THEN
          ReadChar(Source,SourceCh);
          EXIT;
        END;
        IF EOS(Source) THEN EXIT END;
        WriteChar(Listing,SourceCh);
        ReadChar(Source,SourceCh);
      END;
    END;
    WriteEndLine;
  END ReadWriteLine;
 
  BEGIN
    IF PassBefore <= 3 THEN NHead := NLineNr + NSpace;
    ELSE NHead := NLineNr + NAddr + NSpace;
    END;
    ErrHead := Stars;
    SourceFile := modFile;
    ListingFile := lstFile;
    Connect(Source,SourceFile,(*WordFlag=*)FALSE); Reset(Source);
    ReadChar(Source,SourceCh);
    Connect(Listing,ListingFile,(*WordFlag=*)FALSE); Reset(Listing);
  END SourceAndListing;
 
 VAR CurrLineNr: CARDINAL; (* number of current line on source *)
 
 (* Finalization *)
 
 PROCEDURE EndStreams;
  BEGIN  
   EndWrite(Listing);
   Disconnect(Control,FALSE);
   Disconnect(Source,FALSE);
   Disconnect(Listing,FALSE);
  END EndStreams;

 (* Main programme *)

BEGIN (* MCListing *)
  WriteEndLine;
  CurrLineNr := 0;
  ReadContr;
  WHILE NOT Contr.Eof DO
   IF Contr.IsLineHead THEN (* line head *)
    WITH Contr DO
     INC(CurrLineNr);
     IF CurrLineNr # LineNr THEN WriteFailMess(IncorrLineNr) END;
     WriteNormHead(LineNr,Addr);
    END;
    ReadWriteLine;
    REPEAT ReadContr;
    UNTIL Contr.Eof OR NOT Contr.IsLineHead
      OR (Contr.LineNr#CurrLineNr);
   ELSE (* error message *)
    WriteErrHead;
    LastErrPos := 0; LastErrNr := 0;
    REPEAT
     WITH Contr DO
      IF (ErrPos#LastErrPos) OR (ErrNr#LastErrNr) THEN
       WriteErrFlag(ErrPos); WriteErrNr(ErrNr);
       LastErrPos := ErrPos; LastErrNr := ErrNr;
      END;
     END;
     ReadContr;
    UNTIL Contr.Eof OR Contr.IsLineHead;
    WriteEndLine;
   END;
  END;
  WHILE NOT EOS(Source) DO
   INC(CurrLineNr);
   WriteNormHead(CurrLineNr,0); ReadWriteLine;
  END;
  EndStreams;
END MCListing.
