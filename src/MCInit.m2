(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*     running on ENDLICH                *
*                                       *
*                                       *
*     MCInit:                           *
*                                       *
*     initialisation part               *
*     of the Modula-2 compiler          *
*                                       *
*     Version C18 of 04.06.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
*     revision afb 8/83                 *
*                                       *
*                                       *
****************************************)

MODULE MCInit;   (* LG *)

  FROM FileSystem IMPORT Create, Close, Response;
  FROM FilePool IMPORT
    FilePointer, GetFileNumber, GetFileFromPool, ReturnFileNumber;
  FROM FileLookup IMPORT LookupFile;
  FROM FileNames IMPORT Identifiers, IdentifierPosition;
  FROM Options IMPORT GetOption;
  FROM WriteStrings IMPORT WriteString, WriteLn;
  FROM MCPublic IMPORT
    il1File, il2File, ascFile,
    modFile, lstFile, symFile, refFile, objFile,
    Compilerstatus, Statset, compstat,
    comptime, symfileextension;
  FROM MCFileNames IMPORT
    FileName, modName, symName, refName, objName, lstName;
  IMPORT
    SYSTEM;

  CONST
    null = 0C;
    optionlength = 10;

  TYPE
    String = ARRAY [0..19] OF CHAR;
    OptionKind = (nooption,
                  query, noquery,  (* explicit query for symbol files *)
                  list, nolist,    (* generation of a listing file *)
                  small, large,    (* size of compiled module, work files in memory *)
                  version,         (* display of compiler version *)
                  illegal);
    OptionText = ARRAY [0 .. optionlength-1] OF CHAR;
    Option = RECORD
               minlength : CARDINAL;
               maxlength : CARDINAL;
               text : OptionText;
             END;

  VAR
    compiling : BOOLEAN; (* compiler may run *)
    options : ARRAY OptionKind OF Option;
    qopt, mopt, lopt, vopt : OptionKind;

  PROCEDURE InitOption(optkind: OptionKind; opttext: OptionText;
                       min, max: CARDINAL);
  BEGIN
    WITH options[optkind] DO
      text := opttext;
      minlength := min;
      maxlength := max;
    END;
  END InitOption;

  PROCEDURE InterpreteOptions(VAR illegaloptions: BOOLEAN);
    VAR opttext : OptionText;
        optlength : CARDINAL;
        optkind : OptionKind;

    PROCEDURE FindOption(VAR opttext: OptionText; optlength: CARDINAL;
                         VAR optkind: OptionKind);
      VAR opk : OptionKind;
          found : BOOLEAN;
          ix : CARDINAL;
    BEGIN
      opk := nooption; INC(opk);
      found := FALSE;
      WHILE NOT found AND (opk < illegal) DO
        WITH options[opk] DO
          IF (minlength<=optlength) AND (optlength<=maxlength) THEN
            ix := 0;
            WHILE (ix < optlength) AND (opttext[ix] = text[ix]) DO
              INC(ix);
            END;
            found := ix = optlength;
          END;
        END;
        IF NOT found THEN INC(opk) END;
      END;
      optkind := opk;
    END FindOption;

  BEGIN (* InterpreteOptions *)
    illegaloptions := FALSE;
    GetOption(opttext,optlength);
    WHILE optlength > 0 DO
      FindOption(opttext,optlength,optkind);
      CASE optkind OF
        query, noquery : qopt := optkind;
       |small, large : mopt := optkind;
       |list, nolist : lopt := optkind;
       |version : vopt := optkind;
       |illegal :
          WriteString(" ---- illegal option: ");
          WriteString(opttext);
          WriteLn;
          illegaloptions := TRUE;
      END; (* CASE *)
      GetOption(opttext,optlength);
    END;
  END InterpreteOptions;

  PROCEDURE InitCompilation;

    TYPE DeviceName = ARRAY [0..7] OF CHAR;

    VAR str, extension : String;
        workdevice : DeviceName;
        illegal : BOOLEAN;
        ok : BOOLEAN;
        ix : CARDINAL;
        idents : CARDINAL;
        extpos : CARDINAL;

    PROCEDURE CreateFile(VAR fnum: CARDINAL; device: DeviceName);
      VAR fp : FilePointer;
          err : BOOLEAN;
    BEGIN
      GetFileNumber(fnum,err);
      GetFileFromPool(fnum,fp,err);
      Create(fp^,device);
      IF fp^.res <> done THEN
        compiling := FALSE;
        WriteString(" ---- file creation failed");
        WriteLn;
        ReturnFileNumber(fnum);
      END;
    END CreateFile;

    PROCEDURE ReturnFile(fnum: CARDINAL);
      VAR fp : FilePointer;
          err : BOOLEAN;
    BEGIN
      GetFileFromPool(fnum,fp,err);
      IF NOT err THEN (* close file *)
        Close(fp^);
        ReturnFileNumber(fnum);
      END;
    END ReturnFile;

    PROCEDURE FixName(ext: ARRAY OF CHAR; VAR fname: FileName);
      VAR ix, pos : CARDINAL;
    BEGIN
      ix := 0;
      pos := extpos;
      fname := modName;
      WHILE (ix <= HIGH(ext)) AND (ext[ix] <> null) AND
            (pos <= HIGH(fname)) DO
        fname[pos] := ext[ix];
        INC(pos);
        INC(ix);
      END;
      IF pos <= HIGH(fname) THEN fname[pos] := null END;
    END FixName;

  BEGIN
    comptime[0] := 0;
    comptime[1] := 0;
    comptime[2] := 0;
    str := "source file";
    extension := "m2";
    (* read source file name *)
    LOOP
      (* reset options *)
      qopt := noquery;
      mopt := large;
      lopt := nolist;
      vopt := nooption;
      (* lookup for source file *)
      LookupFile(str,extension,modFile,TRUE,FALSE,TRUE,modName,ok);
      IF ok THEN
        InterpreteOptions(illegal);
        IF NOT illegal THEN (* test on extension *)
          idents := Identifiers(modName);
          IF idents >= 2 THEN
            extpos := IdentifierPosition(modName,idents - 1);
          ELSE
            illegal := TRUE;
            WriteString(" ---- extension missing");
            WriteLn;
          END;
        END;
        IF illegal THEN ReturnFile(modFile);
        ELSE
          compiling := TRUE;
          EXIT;
        END;
      ELSE
        compiling := FALSE;
        WriteString(" ---- no compilation");
        WriteLn;
        EXIT;
      END;
    END; (* LOOP *)
  
    IF compiling THEN
      (* use option information *)
      IF qopt = query THEN INCL(compstat,querys) END;
      IF mopt = small THEN workdevice := "MF" ELSE workdevice := "DK" END;
      IF lopt = list THEN INCL(compstat,listings) END;
      IF vopt = version THEN (* display version information *)
        WriteString(" MODULA-2 compiler for Lilith Computer");
        WriteLn;
        WriteString(" version C18 of February 1981");
        WriteLn;
        WriteString(" for system MEDOS - 2");
        WriteLn;
        WriteString(" running on Lilith Computer");
        WriteLn;
      END;
      (* create compiler work files *)
      CreateFile(il1File,workdevice); (* interpass file *)
      CreateFile(il2File,workdevice); (* interpass file *)
      CreateFile(ascFile,workdevice); (* identifier table file *)
      (* create generated files *)
      CreateFile(symFile,workdevice); (* symbol file *)
      CreateFile(refFile,workdevice); (* reference file *)
      CreateFile(objFile,workdevice); (* object code file *)
      IF lopt = list THEN CreateFile(lstFile,workdevice) END; (* listing file *)
      (* fix file names of generated files with new extensions *)
      symfileextension := "sy";
      FixName(symfileextension,symName);
      extension[1] := 0C;
      extension[0] := "r"; FixName(extension,refName);
      extension[0] := "o"; FixName(extension,objName);
      extension[0] := "l"; FixName(extension,lstName);
      IF NOT compiling THEN (* return all files *)
        ReturnFile(modFile);
        ReturnFile(il1File);
        ReturnFile(il2File);
        ReturnFile(ascFile);
        ReturnFile(symFile);
        ReturnFile(refFile);
        ReturnFile(objFile);
        IF lopt = list THEN ReturnFile(lstFile) END;
      END;
    END;
  END InitCompilation;

BEGIN (* MCInit *)
  InitOption(query,"QUERY",1,5);
  InitOption(noquery,"NOQUERY",3,7);
  InitOption(small,"SMALL",2,5);
  InitOption(large,"LARGE",2,5);
  InitOption(list,"LISTING",4,7);
  InitOption(nolist,"NOLISTING",1,9);
  InitOption(version,"VERSION",1,7);
  InitCompilation;
  IF compiling THEN INCL(compstat,compiles) END;
END MCInit.
