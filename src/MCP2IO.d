(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCP2IO:                           *
*                                       * 
*     input / output handling in Pass 2 *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCP2IO;   (* LG *)

  FROM SYSTEM IMPORT WORD;
  FROM MCBase IMPORT Symbol, Spellix, Keyarr;

  EXPORT QUALIFIED
    sy,
    PutSy, PutWord, StopOutput, RestartOutput,
    Error, ErrorLS,
    line, pos, spix, maxspix, val, length,
    GetSy, PutGetSy,
    TermInOut,
    GetModuleKey, DefModStatus,
    AsciiSetPos, AsciiRead,
    SkipConstant, SkipType;

  CONST maxspix = 17777B; (* maximal value of legal spix *)

  VAR sy : Symbol;
      line : CARDINAL;
      pos : CARDINAL;
      spix : Spellix;
      val : CARDINAL;
      length : CARDINAL; (* string length *)

  PROCEDURE PutSy(s : Symbol);
    (* put Symbol ans pos on il2-file *)

  PROCEDURE PutWord(w : WORD);
    (* put word on il2-file *)

  PROCEDURE StopOutput;
  
  PROCEDURE RestartOutput;

  PROCEDURE Error(n: CARDINAL);
    (* no suppression of writing on il2 file *)

  PROCEDURE ErrorLS(n: CARDINAL);

  PROCEDURE GetSy;

  PROCEDURE PutGetSy; 
         
  PROCEDURE TermInOut;

  PROCEDURE GetModuleKey(VAR modkey: Keyarr);

  PROCEDURE DefModStatus;

  PROCEDURE AsciiSetPos(spix: Spellix); 
    (* set position on ASCII file *) 

  PROCEDURE AsciiRead(VAR ch: CHAR);   
    (* read character from ASCII file *) 

  PROCEDURE SkipConstant;
    (* skip a constant in a symbol module *)

  PROCEDURE SkipType;
    (* skip type structures in a symbol module *)

END MCP2IO.
