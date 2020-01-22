(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCP3Ident:                        *
*                                       * 
*     Identifier handling in Pass 3     *
*                                       * 
*     Version C18 of 10.07.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

IMPLEMENTATION MODULE MCP3Ident;     (* LG *)

  IMPORT Storage, MCBase, MCP3IO;

  MODULE SearchSystem;

    FROM MCBase IMPORT Spellix, Idptr, Idclass;
    FROM MCP3IO IMPORT spix;

    EXPORT Locate, Search;

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
      (* The name spix is taken from MCP3IO. *)
      (* The result (possibly NIL) is        *)
      (* assigned to ip.                     *) 
    BEGIN
      Search(list,spix,ip); (* spix from MCP3IO *)
    END Locate;

  END SearchSystem; 

  MODULE AmongSystem;

    FROM MCBase IMPORT Stptr, Structform, Stset;

    EXPORT FAmong;

    PROCEDURE FAmong(sp: Stptr; forms: Stset): BOOLEAN;
      VAR amo : BOOLEAN;
    BEGIN (* form of referenced structure is among forms *) 
      amo := FALSE;
      IF sp <> NIL THEN
        WITH sp^ DO
          IF form IN forms THEN amo := TRUE
          ELSIF form = subranges THEN amo := FAmong(scalp,forms)
          END
        END
      END;
      RETURN amo
    END FAmong;

  END AmongSystem;

  MODULE ImpListSystem;

    FROM Storage IMPORT ALLOCATE, DEALLOCATE;
    FROM MCBase IMPORT
      Idptr, Stptr, Idclass, Structform,
      Stset, Spellix, Listptr, root;
    FROM MCP3IO IMPORT spix, Error;
    FROM SearchSystem IMPORT Search;
    FROM AmongSystem IMPORT FAmong;

    EXPORT
      NewImpList, TermImpList, EnterImpList, ImpSearch,
      DisposeImpList;   
 
    VAR implist : Listptr; 
 
    PROCEDURE NewImpList(imp: Listptr); 
    BEGIN 
      implist := imp; 
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
    BEGIN (* spix from MCP3IO *)
      IF mp <> NIL THEN imp := mp^.impp ELSE imp := NIL END;
      lip := NIL; 
      WHILE (lip = NIL) AND (imp <> NIL) DO 
        WITH imp^.element^ DO
          IF name = spix THEN
            lip := imp^.element; 
          ELSIF (klass = mods) AND NOT qualexp THEN
            Search(expp,spix,lip);
          ELSIF (klass = types) AND FAmong(idtyp,Stset{enums})THEN 
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

    PROCEDURE DisposeImpList(VAR imp: Listptr);
      (* dispose the whole importlist of a module *)
      VAR imp1 : Listptr;
    BEGIN
      WHILE imp <> NIL DO
        imp1 := imp;
        imp := imp^.next;
        DISPOSE(imp1);
      END;
    END DisposeImpList;

  END ImpListSystem;

  MODULE Scope;

    FROM Storage IMPORT ALLOCATE, DEALLOCATE;
    FROM MCBase IMPORT
      Idptr, Stptr, Listptr, Idclass, Structform, Stset;
    FROM MCP3IO IMPORT spix, Error;
    FROM SearchSystem IMPORT Search;
    FROM AmongSystem IMPORT FAmong;
    FROM ImpListSystem IMPORT ImpSearch;

    EXPORT
      SearchId, ExportSearch,
      MarkModScope, ReleaseModScope,
      MarkProcScope, ReleaseProcScope,
      MarkWithScope, ReleaseWithScope, FieldIndex,
      BodyMark, BodyScopes; 

    TYPE Scoperange = [0 .. 15];
         Scopeset = SET OF Scoperange;

    VAR displ : ARRAY Scoperange OF
                  RECORD
                    modp : Idptr; (* reference to module name *)
                    brf : Idptr; 
                    msfirst: Listptr;
                  END;
        level : Scoperange;          (* current level *)
        foundlevel : Scoperange;     (* level on which identifier was found *)
                                     (* set in SearchId, used in FieldIndex *)
        modlevel : Scoperange;       (* level of module scope *)
        bodylevel : Scoperange;      (* level of compiled body *)
        maxlevel : Scoperange;       (* maximal level in body *)
        modlevstack : Scopeset;      (* stack of module levels *)

    PROCEDURE SearchId(VAR ip: Idptr);
      VAR ls : Listptr;
          clev : Scoperange;
      (* Search an identifier in the current scope. *)
      (* The name spix is taken from MCP3IO.        *)
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
      foundlevel := clev;
    END SearchId; 

    PROCEDURE ExportSearch(ep: Idptr; VAR ip: Idptr);
      (* Search an identifier in the         *)
      (* export list of a module.            *)
      (* The name spix is taken from MCP3IO. *)
      VAR lip : Idptr; 
          lep : Idptr; 
          sp : Stptr; 
    BEGIN (* spix from MCP3IO *)
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

    PROCEDURE NewLevel(mp,ip: Idptr; fmsfirst: Listptr);
    BEGIN
      INC(level);
      IF level <= 15 THEN
        WITH displ[level] DO
          modp := mp; brf := ip; msfirst := fmsfirst;
        END;
      ELSE
        Error(79);
      END;
    END NewLevel; 

    PROCEDURE OldLevel;
      VAR mp,mp1 : Listptr;
    BEGIN
      (* dispose ms-table of this block *)
      IF level <= 15 THEN
        mp := displ[level].msfirst;
        WHILE mp <> NIL DO mp1 := mp; mp := mp^.next; DISPOSE(mp1) END;
      END;
      IF level > maxlevel THEN maxlevel := level END;
      DEC(level);
    END OldLevel; 

    PROCEDURE MarkModScope(ip : Idptr);
    BEGIN
      WITH ip^ DO 
        NewLevel(ip,expp,NIL);
        IF level <= 15 THEN
          modlevel := level;
          INCL(modlevstack,modlevel);
        END;
        NewLevel(NIL,locp,msp);
      END
    END MarkModScope;

    PROCEDURE ReleaseModScope;
    BEGIN
      OldLevel;
      IF level <= 15 THEN
        EXCL(modlevstack,modlevel);
        REPEAT DEC(modlevel) UNTIL modlevel IN modlevstack;
      END;
      OldLevel;
    END ReleaseModScope;

    PROCEDURE MarkProcScope(ip: Idptr);
      (* Mark the scope of a procedure *)
    BEGIN
      WITH ip^ DO
        IF klass = mods THEN (* module body *)
          NewLevel(NIL,NIL,NIL);
        ELSE NewLevel(NIL,locp,msp);
        END;
      END;
    END MarkProcScope;

    PROCEDURE ReleaseProcScope;
      (* Release the scope of a procedure *)
    BEGIN
      OldLevel;
    END ReleaseProcScope;

    PROCEDURE MarkWithScope(ip: Idptr);
      (* mark the scope of a with statement *)
    BEGIN
      NewLevel(NIL,ip,NIL);
    END MarkWithScope;

    PROCEDURE ReleaseWithScope;
      (* Release the scope of a with statement *)
    BEGIN
      OldLevel;
    END ReleaseWithScope;

    PROCEDURE FieldIndex(): CARDINAL;
      (* Return the nestlevel of a field in a with statement *)
    BEGIN
      RETURN foundlevel - bodylevel;
    END FieldIndex;

    PROCEDURE BodyMark; 
      (* mark level of the body of a procedure *)
    BEGIN
      bodylevel := level;
      maxlevel := bodylevel;
    END BodyMark;

    PROCEDURE BodyScopes(): CARDINAL;
      (* return maximal nesting in body *)
    BEGIN
      RETURN maxlevel - bodylevel;
    END BodyScopes;

  BEGIN (* Scope *)
    level := 0; modlevel := 0; modlevstack := Scopeset{0};
    bodylevel := 0; maxlevel := 0;
    WITH displ[level] DO
      modp := NIL; brf := NIL; msfirst := NIL
    END;
  END Scope;

END MCP3Ident.
