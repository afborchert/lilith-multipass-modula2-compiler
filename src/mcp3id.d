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

DEFINITION MODULE MCP3Ident;   (* LG *)

  FROM MCBase IMPORT
    Idptr, Stptr, Stset, Listptr, Spellix;
  FROM MCP3IO IMPORT spix;
   
  EXPORT QUALIFIED
    Locate,
    FAmong,
    NewImpList, TermImpList, EnterImpList, DisposeImpList,
    MarkModScope, ReleaseModScope,
    MarkProcScope, ReleaseProcScope,
    MarkWithScope, ReleaseWithScope, FieldIndex, 
    BodyMark, BodyScopes,
    SearchId, ExportSearch;

  PROCEDURE Locate(list: Idptr; VAR ip: Idptr); 
    (* Search an identifier in list.       *)  
    (* The name spix is taken from MCP3IO. *)
    (* The result (possibly NIL) is        *)
    (* assigned to ip.                     *) 

  PROCEDURE FAmong(sp: Stptr; forms: Stset): BOOLEAN;
   (* form of referenced structure is among forms *)

  PROCEDURE NewImpList(imp: Listptr);
    (* Initialisation of a new import list. *) 
    (* An existing import list belonging to *)
    (* the same module is disposed.         *)

  PROCEDURE TermImpList(VAR imp: Listptr);
    (* Termination of the new import list. *)
    (* The new list is assigned to impp.   *)

  PROCEDURE EnterImpList(ip: Idptr);
    (* The identifier ip is entered into the import list. *)

  PROCEDURE DisposeImpList(VAR imp: Listptr);
    (* Dispose the whole import list of a module. *)

  PROCEDURE MarkModScope(ip: Idptr);  
    (* Mark the scope of a module. *) 

  PROCEDURE ReleaseModScope; 
    (* Release the scope of a module. *)  

  PROCEDURE MarkProcScope(ip: Idptr);
    (* Mark the scope of a procedure. *) 
 
  PROCEDURE ReleaseProcScope;                  
    (* Release the scope of a procedure. *)
 
  PROCEDURE MarkWithScope(ip: Idptr);
    (* Mark the scope of a with statement *)
 
  PROCEDURE ReleaseWithScope; 
    (* Release the scope of a with statement *) 

  PROCEDURE FieldIndex(): CARDINAL; 
    (* Return the nesting level of a in a with statement *)
 
  PROCEDURE BodyMark;
    (* mark scope level of compiled body *)

  PROCEDURE BodyScopes(): CARDINAL;
    (* return number of scopes nested in compiled body *)

  PROCEDURE SearchId(VAR ip: Idptr);  
    (* Search an identifier in all scopes. *) 
    (* The name spix is taken from MCP3IO. *) 

  PROCEDURE ExportSearch(explist: Idptr; VAR ip: Idptr); 
    (* Search an identifier in the         *)
    (* export list of a module.            *)
    (* The name spix is taken from MCP3IO. *)  

END MCP3Ident.
