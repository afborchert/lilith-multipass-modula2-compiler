(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCP2Ident:                        *
*                                       * 
*     Identifier handling in Pass 2     *
*                                       * 
*     Version C18 of 10.07.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

IMPLEMENTATION MODULE MCP2Ident;   (* LG *)

   (* $T- *)

  FROM MCBase IMPORT Stptr, Structform, Stset;
  IMPORT Storage, MCBase, MCP2IO;

  PROCEDURE FAmong(sp: Stptr; forms: Stset): BOOLEAN;
  BEGIN
    IF sp = NIL THEN RETURN FALSE END;
    WITH sp^ DO
      RETURN (form IN forms) OR
             (form = subranges) AND FAmong(scalp,forms);
    END;
  END FAmong;

  MODULE SearchSystem;

    FROM MCP2IO IMPORT spix;
    FROM MCBase IMPORT
      Idptr, Spellix, Idclass, Structform; 

    EXPORT Search, Locate; 

    PROCEDURE Search(list: Idptr; spix: Spellix; VAR ip: Idptr); 
      (* search in list until found or spix < name *)
    BEGIN
      ip := NIL;
      WHILE list <> NIL DO
        WITH list^ DO
          IF spix > name THEN list := link
          ELSIF spix < name THEN list := NIL
          ELSE (* spix = name *)
            IF klass = indrct THEN ip := nxtidp; (* real entry *)
            ELSE ip := list;
            END;
            list := link; (* find last entry with the same name *)
          END
        END (* WITH *)
      END
    END Search; 

    PROCEDURE Locate(list: Idptr; VAR ip: Idptr);
      (* Search an identifier in list.       *)  
      (* The name spix is taken from MCP2IO. *)
      (* The result (possibly NIL) is        *)
      (* assigned to ip.                     *) 
    BEGIN
      Search(list,spix,ip); (* spix from MCP2IO *)
    END Locate;

  END SearchSystem; 

  MODULE ImpListSystem;

    FROM Storage IMPORT ALLOCATE, DEALLOCATE;
    FROM MCP2IO IMPORT spix, Error;
    FROM MCBase IMPORT
      Idptr, Stptr, Idclass, Structform, Stset,
      Spellix, root, Listptr;
    IMPORT SearchSystem, FAmong;

    EXPORT NewImpList, TermImpList, EnterImpList, ImpSearch;   
 
    VAR implist : Listptr; 
 
    PROCEDURE NewImpList(imp: Listptr); 
      VAR imp1 : Listptr; 
    BEGIN 
      WHILE imp <> NIL DO (* dispose old import list *)
        imp1 := imp; 
        imp := imp^.next; 
        DISPOSE(imp1); 
      END; 
      implist := NIL; 
    END NewImpList; 

    PROCEDURE TermImpList(VAR imp: Listptr); 
    BEGIN 
      imp := implist; 
    END TermImpList; 
 
    PROCEDURE EnterImpList(ip: Idptr);
      (* enter identifier in import list *)
      VAR newimp : Listptr; 
          collision : BOOLEAN; 
 
      PROCEDURE OneCheck(spix: Spellix); 
        (* check on second import list entry of identifier spix *) 
        VAR lip : Idptr; 
            imp : Listptr;   
      BEGIN 
        IF NOT collision THEN  
          imp := implist; lip := NIL; 
          WHILE (imp <> NIL) AND NOT collision DO 
            WITH imp^.element^ DO 
              IF name = spix THEN lip := imp^.element; 
              ELSIF (klass=mods) AND NOT qualexp THEN
                Search(expp,spix,lip); 
              ELSIF (klass=types) AND (idtyp<>NIL) THEN  
                IF idtyp^.form=enums THEN
                  Search(idtyp^.fcstp,spix,lip);
                END; 
              END; 
            END; (* WITH *)  
            imp := imp^.next;
            collision := lip <> NIL; 
          END; (* WHILE *)  
        END; 
      END OneCheck;   

      PROCEDURE ListCheck(ip: Idptr); 
        (* check on second import-list entry of *)
        (* identifiers in list ip               *)
        (* enter also nested modules or         *)
        (* enumeration types in import list     *)
        VAR lip : Idptr;
      BEGIN 
        WHILE ip <> NIL DO   
          lip := ip; 
          IF lip^.klass = indrct THEN lip := lip^.nxtidp END; 
          WITH lip^ DO      
            IF (klass = mods) AND NOT qualexp THEN EnterImpList(lip);
            ELSIF (klass = types) AND FAmong(idtyp,Stset{enums})THEN  
              EnterImpList(lip); 
            ELSE OneCheck(name);
            END; 
          END; (* WITH *)   
          ip := ip^.link; 
        END; 
      END ListCheck;   
 
    BEGIN
      collision := FALSE; 
      WITH ip^ DO 
        OneCheck(name);
        IF (klass = mods) AND NOT qualexp THEN ListCheck(expp)
        ELSIF (klass = types) AND (idtyp <> NIL) THEN 
          IF idtyp^.form = enums THEN ListCheck(idtyp^.fcstp) END; 
        END; 
      END; 
      IF collision THEN Error(70) END;   
      NEW(newimp); 
      WITH newimp^ DO element := ip; next := implist END;  
      implist := newimp; 
    END EnterImpList;

    PROCEDURE ImpSearch(mp: Idptr; VAR ip: Idptr);
      (* search identifier in import list of module mp *)
      VAR imp : Listptr;
          lip : Idptr;
          sp : Stptr;
    BEGIN (* spix from MCP2IO *)
      IF mp <> NIL THEN imp := mp^.impp ELSE imp := NIL END;
      lip := NIL; 
      WHILE (lip = NIL) AND (imp <> NIL) DO 
        WITH imp^.element^ DO
          IF name = spix THEN
            lip := imp^.element; 
          ELSIF (klass = mods) AND NOT qualexp THEN
            Search(expp,spix,lip);
          ELSIF (klass = types) AND FAmong(idtyp,Stset{enums}) THEN 
            sp := idtyp; 
            IF sp^.form = subranges THEN sp := sp^.scalp END; 
            Search(sp^.fcstp,spix,lip);
          END; 
        END; (* WITH *) 
        imp := imp^.next; 
      END; (* WHILE *) 
      IF lip = NIL THEN (* search for pervasives *)
        Search(root^.expp,spix,lip);
      END; 
      ip := lip;
    END ImpSearch;

  END ImpListSystem;

  MODULE Scope; 

    FROM Storage IMPORT ALLOCATE, DEALLOCATE;
    FROM MCP2IO IMPORT spix, Error, ErrorLS;
    FROM MCBase IMPORT
      Idclass, Structform, Stset, Idptr,
      Stptr, Spellix, Listptr;
    IMPORT SearchSystem, ImpListSystem, FAmong; 

    EXPORT
      EnterList, EnterId, EnterForward,
      SearchId, ExportSearch, SymModSearch,
      GlobalKnown,
      MsEntry, SearchInBlock, MarkScope, ReleaseScope;

    CONST maxscope = 15;

    TYPE Scoperange = [0 .. maxscope];
         Scopeset = SET OF Scoperange;
         Forwardptr = POINTER TO Forwardrec;
         Forwardrec = RECORD
                        next : Forwardptr;
                        fwname : Spellix;
                        ptrtp : Stptr;
                      END;

    VAR displ : ARRAY Scoperange OF 
                  RECORD
                    modp : Idptr; (* reference to module name *)
                    brf : Idptr; 
                    msfirst: Listptr; 
                    mslast: Listptr;
                    forwp : Forwardptr;
                  END;
        level: Scoperange;
        modlevel : Scoperange; (* scope level of enclosing module *)
        modlevstack: Scopeset; (* stack of module levels *)
        ismodblock : BOOLEAN;

    PROCEDURE EnterList(VAR rf: Idptr; fip: Idptr);
      VAR lip,lip1: Idptr; nam: Spellix;
    BEGIN 
      (* insert in ordered list *)
      nam := fip^.name; 
      lip := rf;
      WHILE (lip <> NIL) AND (lip^.name <= nam) DO
        IF lip^.name = nam THEN Error(72) END;
        lip1 := lip;
        lip := lip1^.link;
      END;
      fip^.link := lip; 
      IF lip = rf THEN rf := fip ELSE lip1^.link := fip END;
    END EnterList; 
  
    PROCEDURE SearchInBlock(VAR x: Idptr);
      VAR ls : Listptr; 
    BEGIN (* spix from MCP2IO *)
      IF level <= 15 THEN
        Search(displ[level].brf,spix,x);
        IF x = NIL THEN ls := displ[level].msfirst; 
          WHILE (ls <> NIL) AND (x = NIL) DO
            WITH ls^ DO Search(element,spix,x); ls := next END 
          END 
        END;
        IF (x = NIL) AND ismodblock THEN (* module block *)
          ImpSearch(displ[modlevel].modp,x);
          IF x = NIL THEN Search(displ[modlevel].brf,spix,x) END; 
          (* possibly unknown entry in export list *)
        END;
      ELSE x := NIL;
      END;
    END SearchInBlock;

    PROCEDURE EnterId(x: Idptr);
      VAR y: Idptr; 
    BEGIN 
      IF level <= 15 THEN
        SearchInBlock(y);
        IF y = NIL THEN EnterList(displ[level].brf,x)
        ELSIF y^.klass = unknown THEN (* replace export-list entry *)
          WITH y^ DO x^.link := link; 
            IF (link <> NIL) AND (link^.klass = unknown) THEN 
              link^.nxtidp := x;
            END;
            WITH nxtidp^ DO (* is guaranteed to be <> NIL *)
              IF (klass=mods) AND (expp=y) THEN
                (* identifier is first element in exportlist *) 
                displ[modlevel].brf := x; expp := x; 
              ELSE link := x END; 
            END;
          END;
          DISPOSE(y,unknown); 
        ELSE Error(72); EnterList(displ[level].brf,x)
        END;
      END;
    END EnterId;

    PROCEDURE EnterForward(ptrtype: Stptr);
      VAR lfp : Forwardptr;
    BEGIN
      IF level <= 15 THEN
        NEW(lfp);
        WITH lfp^DO
          next := displ[level].forwp;
          fwname := spix; (* from MCP2IO *)
          ptrtp := ptrtype;
        END;
        displ[level].forwp := lfp;
      END;
    END EnterForward;

    PROCEDURE UpdateForwards;
      VAR updatefail : BOOLEAN;
          lfp, lfp1 : Forwardptr;
          oldspix : Spellix;
          ip : Idptr;
    BEGIN
      IF level <= 15 THEN
        updatefail := FALSE;
        oldspix := spix; (* save current spix *)
        lfp := displ[level].forwp;
        WHILE lfp <> NIL DO
          WITH lfp^DO
            spix := fwname;
            SearchId(ip);
            IF (ip = NIL) OR (ip^.klass <> types) THEN
              updatefail := TRUE;
            ELSE
              ptrtp^.elemp := ip^.idtyp;
            END;
            lfp1 := lfp;
            lfp := next;
          END;
          DISPOSE(lfp1);
        END;
        spix := oldspix;
        IF updatefail THEN ErrorLS(90) END;
      END;
    END UpdateForwards;

    PROCEDURE SearchId(VAR ip: Idptr);
      VAR ls : Listptr;
          clev : Scoperange;
      (* Search an identifier in the current scope. *)
      (* The name spix is taken from MCP2IO.        *)
    BEGIN
      ip := NIL;
      IF level <= 15 THEN
        clev := level;
        LOOP
          Search(displ[clev].brf,spix,ip);
          IF ip = NIL THEN
            ls := displ[clev].msfirst; 
            WHILE (ls <> NIL) AND (ip = NIL) DO
              Search(ls^.element,spix,ip); ls := ls^.next;
            END;
          END;
          IF (ip <> NIL) OR (clev = modlevel) THEN EXIT END;
          DEC(clev);
        END;
        IF ip = NIL THEN ImpSearch(displ[modlevel].modp,ip) END;
      END;
    END SearchId;

    PROCEDURE ExportSearch(ep: Idptr; VAR ip: Idptr);
      (* Search an identifier in the         *)
      (* export list of a module.            *)
      (* The name spix is taken from MCP2IO. *)
      VAR lip : Idptr; 
          lep : Idptr; 
          sp : Stptr; 
    BEGIN (* spix from MCP2IO *)
      Search(ep,spix,lip); 
      IF lip = NIL THEN (*search in enumeration type or nested module*) 
        WHILE (lip = NIL) AND (ep <> NIL) DO 
          lep := ep;   
          IF lep^.klass = indrct THEN lep := lep^.nxtidp END; 
          WITH lep^ DO   
            IF (klass = mods) AND NOT qualexp THEN
              ExportSearch(expp,lip);  
            ELSIF (klass = types) AND FAmong(idtyp,Stset{enums}) THEN 
              sp := idtyp; 
              IF sp^.form = subranges THEN sp := sp^.scalp END;  
              Search(sp^.fcstp,spix,lip)
            END; 
          END; (* WITH *) 
        ep := ep^.link; 
        END; (* WHILE *) 
      END; 
      ip := lip;
    END ExportSearch;

    PROCEDURE SymModSearch(VAR x: Idptr);
      (* search identifier belonging to symbol module *)
      VAR y : Idptr;
    BEGIN (* spix from MCP2IO *)
      y := NIL;
      IF level <= 15 THEN
        IF ismodblock THEN Search(displ[modlevel].brf,spix,y) END;
        IF y = NIL THEN Search(displ[level].brf,spix,y)
        ELSIF y^.klass = unknown THEN y := NIL
        END;
      END;
      x := y;
    END SymModSearch;

    PROCEDURE GlobalKnown(spx: Spellix): BOOLEAN;
      (* check name spx for possible implementation *)
      (* i.e. it must be known at global level of   *)
      (* an implementation module; the name may be  *)        
      (* exported from a nested module              *)
      VAR clev : Scoperange;
          x : Idptr;
          stack : ARRAY [0..9] OF Spellix; (* possibly exported names *)
          ind, high : [0..10];
    BEGIN (* assume nested modules only *)
      IF level > 15 THEN RETURN FALSE END;
      high := 0; 
      clev := level - 1; (* level of exportlist *)
      WHILE clev > 1 DO (* do not search on global level *)
        WITH displ[clev] DO 
          IF modp^.qualexp THEN RETURN FALSE END; 
          Search(brf,spx,x); (* search spx in exportlist *) 
          IF x = NIL THEN (* search elements from stack *) 
            ind := 0; 
            WHILE ind < high DO   
              Search(brf,stack[ind],x); 
              INC(ind); 
              IF x <> NIL THEN high := ind END; 
            END; 
            IF x = NIL THEN RETURN FALSE END; (* not global known *) 
          ELSE (* enter spx in stack *)  
            stack[high] := spx;  
            INC(high);   
          END; 
          spx := modp^.name; (* module name is possibly exported *) 
        END; 
        DEC(clev,2); 
      END; 
      RETURN TRUE 
    END GlobalKnown; 

    PROCEDURE NewLevel(mp,rf : Idptr; fms: Listptr); 
    BEGIN
      INC(level); 
      IF level <= 15 THEN
        WITH displ[level] DO
          modp := mp;
          brf := rf; msfirst := fms;
          IF fms <> NIL THEN
            WHILE fms^.next <> NIL DO fms := fms^.next END;
          END;
          mslast := fms;
          forwp := NIL;
        END;
      ELSE Error(79);
      END;
    END NewLevel; 

    PROCEDURE OldLevel(VAR rf: Idptr; VAR fms: Listptr); 
    BEGIN
      IF level <= 15 THEN
        WITH displ[level] DO 
          rf := brf; fms := msfirst;
        END;
      END;
      DEC(level); 
    END OldLevel; 

    PROCEDURE MarkScope(ip : Idptr);
    BEGIN
      WITH ip^ DO
        IF klass = mods THEN
          NewLevel(ip,expp,NIL); (* level of exported identifiers *)
          IF level <= 15 THEN
            modlevel := level;
            INCL(modlevstack,modlevel);
          END;
          ismodblock := TRUE;
        ELSE ismodblock := FALSE;
        END;
        NewLevel(NIL,locp,msp); (* level of local identifiers *)
      END;
    END MarkScope; 

    PROCEDURE ReleaseScope(ip: Idptr); 
      VAR dummyid : Idptr;
          dummyms : Listptr;
    BEGIN
      UpdateForwards;
      WITH ip^ DO
        OldLevel(locp,msp);
        IF klass = mods THEN
          IF level <= 15 THEN
            EXCL(modlevstack,modlevel);
            REPEAT DEC(modlevel) UNTIL modlevel IN modlevstack;
          END;
          OldLevel(dummyid,dummyms);
        END;
        ismodblock := level = modlevel + 1;
      END;
    END ReleaseScope;

    PROCEDURE MsEntry(fip: Idptr);
      VAR p : Listptr;
          x : Idptr;
    BEGIN
      IF (fip <> NIL) AND (level <= 15) THEN
        NEW(p);
        WITH p^ DO element := fip; next := NIL END;
        WITH displ[level] DO (* ms-table entry *)
          IF msfirst = NIL THEN msfirst := p ELSE mslast^.next := p END;
          mslast := p
        END;
        IF ismodblock THEN (* check indirect exports on export-list *)
          WHILE fip <> NIL DO 
            Search(displ[modlevel].brf,fip^.name,x);
            IF x <> NIL THEN
              WITH x^ DO
                IF klass = unknown THEN
                  klass := indrct;
                  IF fip^.klass = indrct THEN nxtidp := fip^.nxtidp
                  ELSE nxtidp := fip
                  END
                ELSE Error(72);
                END 
              END 
            END;
            fip := fip^.link;
          END (* WHILE *)
        END
      END
    END MsEntry;

  BEGIN (* Scope *)
    level := 0; modlevel := 0;
    modlevstack := Scopeset{0};
    ismodblock := FALSE; 
    WITH displ[level] DO modp := NIL; brf := NIL; msfirst := NIL END;
  END Scope;

END MCP2Ident.
