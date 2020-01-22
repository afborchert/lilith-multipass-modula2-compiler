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
*     Version C18 of 08.07.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCP2Ident;     (* LG *)

  FROM MCBase IMPORT Idptr, Stptr, Listptr, Spellix;
  FROM MCP2IO IMPORT spix;
   
  EXPORT QUALIFIED
    Locate,
    NewImpList, TermImpList, EnterImpList,
    MarkScope, ReleaseScope,
    MsEntry,
    EnterList, EnterId, EnterForward,
    SearchInBlock, SearchId, 
    ExportSearch, SymModSearch, GlobalKnown;

  PROCEDURE Locate(list: Idptr; VAR ip: Idptr); 
    (* Search an identifier in list.       *)  
    (* The name spix is taken from MCP2IO. *)
    (* The result (possibly NIL) is        *)
    (* assigned to ip.                     *) 

  PROCEDURE NewImpList(imp: Listptr);
    (* Initialisation of a new import list. *) 
    (* An existing import list belonging to *)
    (* the same module is disposed.         *)

  PROCEDURE TermImpList(VAR imp: Listptr);
    (* Termination of the new import list. *)
    (* The new list is assigned to impp.   *)

  PROCEDURE EnterImpList(ip: Idptr);
    (* The identifier ip is entered into the import list. *)

  PROCEDURE MarkScope(ip: Idptr);  
    (* Mark the scope of a module or a procedure. *) 

  PROCEDURE ReleaseScope(ip: Idptr); 
    (* Release the scope of a module or a procedure. *)  

  PROCEDURE MsEntry(list: Idptr);
    (* Enter a list into the mslist of the current scope *)
 
  PROCEDURE EnterList(VAR list: Idptr; ip: Idptr); 
    (* Enter identifier ip into list *) 

  PROCEDURE EnterId(ip: Idptr); 
    (* Enter identifier ip into list of current scope. *) 

  PROCEDURE EnterForward(ptrtype: Stptr);
    (* Enter a forward reference of a pointer type *)
    (* into the forward list of the current scope. *)
    (* The name spix is taken from MCP2IO.         *) 

  PROCEDURE SearchInBlock(VAR ip: Idptr);  
    (* Search an identifier in the current scope. *)
    (* The name spix is taken from MCP2IO.        *) 
 
  PROCEDURE SearchId(VAR ip: Idptr);  
    (* Search an identifier in all scopes. *) 
    (* The name spix is taken from MCP2IO. *) 

  PROCEDURE ExportSearch(explist: Idptr; VAR ip: Idptr); 
    (* Search an identifier in the         *)
    (* export list of a module.            *)
    (* The name spix is taken from MCP2IO. *)  

  PROCEDURE SymModSearch(VAR ip: Idptr); 
    (* Search an identifier in the scope   *)
    (* of a symbolic module.               *) 
    (* The name spix is taken from MCP2IO. *) 

  PROCEDURE GlobalKnown(spix: Spellix): BOOLEAN; 
    (* Check whether the name spix is known on the *) 
    (* global level of an implementation module.   *)

END MCP2Ident.
