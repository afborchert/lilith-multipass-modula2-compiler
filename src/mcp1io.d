(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCP1IO:                           *
*                                       * 
*     input / output handling in Pass 1 *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCP1IO;        (* LG *)
 
  FROM MCBase IMPORT Symbol, Spellix, Constval;
   
  EXPORT QUALIFIED
    sy, val, length, spix,
    PutS, PutSy, PutSyVal, PutIdent, Error,
    InitSave, StopSave, RestartSave, ReleaseSys,
    GetSy,
    GetSeparateModule,
    GetDmId, String14, HashIdent, EnterResWord,
    InitInOut, TermInOut;
 
 
  VAR sy : Symbol;
      val : Constval;
      length: CARDINAL;
      spix : Spellix; (* index generated to each identifier *)

  TYPE String14 = ARRAY [0 .. 13] OF CHAR;
 
  PROCEDURE PutS;
    (* put current symbol *)

  PROCEDURE PutSy(sy: Symbol);
    (* put symbol *)

  PROCEDURE PutSyVal(sy: Symbol; val: CARDINAL);
    (* put symbol and value *)

  PROCEDURE PutIdent(spix: Spellix);
    (* put symbol ident and spix *)

  PROCEDURE Error(n: CARDINAL);
    (* put error with number n *)

  PROCEDURE InitSave;
    (* output must be saved *)

  PROCEDURE StopSave;
    (* stop saving output *)

  PROCEDURE RestartSave;
    (* restart saving output *)

  PROCEDURE ReleaseSys;
    (* release saved output *)

  PROCEDURE GetSy;
    (* get next symbol *)

  PROCEDURE GetSeparateModule;
    (* get symbol file belonging to module with current spix *)

  PROCEDURE GetDmId;
    (* generate a dummy spix for an inexisting identifier *)

  PROCEDURE HashIdent(VAR str: String14);
    (* enter identifier into identifier table *)
    (* an index  spix  is generated *)
 
  PROCEDURE EnterResWord(str: String14; sy: Symbol);
    (* enter reserved word with his symbol *)
 
  PROCEDURE InitInOut;
    (* initialisation of input- and output-streams *)

  PROCEDURE TermInOut;
    (* termination of input- and output-streams *)

END MCP1IO.
