DEFINITION MODULE MCP4Global;   (* Ch. Jacobi 5.4.81 *)

(* global variables and procedures 
   for Pass 4 of the Compiler for the Lilith Computer *)

  FROM SYSTEM IMPORT WORD;
  FROM MCBase IMPORT Symbol, Idptr, Stptr, Stringptr;

  EXPORT QUALIFIED 
    loadAddress, level,
    loadCount, spPosition, 
    blockNptr, 
    WriteCodeWord, WriteIntermediateWord, CloseIO,
    GetSymbol, 
    sy, val,  
    nptr, cstPtr, cString, controlRangeCheck, arithmeticRangeCheck,
    Error, CompilerError, Assert;


(* MODULE io; *)

     PROCEDURE WriteCodeWord(w: WORD);
     PROCEDURE WriteIntermediateWord(w: WORD);
     PROCEDURE CloseIO;

(* END io. *)
 

(* MODULE Scanner; *)

  PROCEDURE GetSymbol;   

  VAR 
    sy:      Symbol;      (* Symbol kind: set at each Symbol *)
    val:     INTEGER;     (* value: set for sy = option.
                             offset: set for sy = field *)
    nptr:    Idptr;       (* pointer to identifier: set for sy IN
                             [namesy,modulesy,proceduresy] *)  
    cstPtr:  Stptr;       (* pointer to type of constant:
                             set for sy = anycon *)  
    cString: Stringptr;   (* value or pointer to value of constant:
                             set for sy = anycon *)
    controlRangeCheck: BOOLEAN;    (* array bound, case index check *)
    arithmeticRangeCheck: BOOLEAN; (* range check *)

(* END Scanner. *)


(* MODULE Globals; *)
 
  VAR 
    loadAddress: CARDINAL;  (* module relative *)
    level: CARDINAL;
    loadCount: CARDINAL;    (* currend size of expression stack*)
    spPosition: INTEGER;    (* relative position of the SP *)
    blockNptr: Idptr;       (* descriptor of current block *)

  PROCEDURE Error(n: INTEGER);   
  
  PROCEDURE CompilerError;  

  PROCEDURE Assert (fb: BOOLEAN);  
    (* a call of Assert is exactly one line length;
       to eliminate these tests delete all lines where the
       string "Assert(" occurs [no blank between "t" and "("!] *)

(* END Globals. *)

END MCP4Global.
