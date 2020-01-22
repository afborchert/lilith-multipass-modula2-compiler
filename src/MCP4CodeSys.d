DEFINITION MODULE MCP4CodeSys;      (* Ch. Jacobi 5.4.81 *)

(* code generation elementary procedures 
   for Pass 4 of the Compiler for the Lilith Computer *)

  FROM SYSTEM IMPORT WORD;
  FROM MCBase IMPORT Idptr, Stptr, Stringptr;

  EXPORT QUALIFIED 
   
    (*Codesys*)
         PC,
         Emit, Emit2, 
         Insert, Insert2, 
         MoveCode,
         MarkShort, UpdateShort,
         MarkLong,  UpdateLong,
         LinkageMark, 
         PutConstString, InitCodeSys, 
        
    (*SimpleCode*)
         Emit2GivePC, Fixup2,
         EmitPacked, EmitLI, EmitLSW,
         EmitSSW, EmitCL, UAddToTop, IAddToTop, UConstMul,
         UConstDiv, UConstMod,

    (*TypeSystem*)
         IsArrayType, ByteSize, IsSetType,

    (*BlockSystem*)
         GenBlockEntry, GenBlockReturn, TerminateBlock,
         CodeBody, GenCodeBlock;



(*DEFINITION MODULE CodeSys;*)

  VAR PC: INTEGER; (*read only*)
 
  PROCEDURE Emit(i: WORD);
  PROCEDURE Emit2(i: WORD);

  PROCEDURE Insert(at: CARDINAL; i: WORD);
  PROCEDURE Insert2(at: CARDINAL; i: WORD);
   
  PROCEDURE MoveCode(from, to, displacement: CARDINAL);
    (* decrements pc if necessary *)

  PROCEDURE MarkShort(VAR lastPc: INTEGER);
    (* insert a short jump in a list structure. 
       to start a new mark-list: use 0 for lastPc. 
       inhibits moving of the current code location until the
       corresponding update is called *)
  
  PROCEDURE UpdateShort(lastPc: INTEGER); 
    (* Update list of short jumps *)
                                             (* the MarkShort, UpdateShort pair 
                                                is used for short circuit AND and OR;
                                                no trackkeeping of code moves *)

  PROCEDURE MarkLong(VAR lastPc: INTEGER);
    (* insert a long jump in a linear list. 
       to start a new mark-list: use 0 for lastPc. *) 
  
  PROCEDURE UpdateLong(lastPc: INTEGER);
    (* Update list of long jumps *) 
                                             (* the MarkLong, UpdateLong pair 
                                                is used for EXIT statements;
                                                keeps track of code moving  *)

  

  PROCEDURE LinkageMark;
    (* keeps track of code moving *)


  PROCEDURE PutConstString(stringTypePtr: Stptr; string: Stringptr);
    (* Determine if string has already been generated,
       else generate template of string *)

  PROCEDURE InitCodeSys;
    (* because of exporting PC the module needs explicite initialization
       to allow circular imports with controlled initialization *)

(*END CodeSys;*)


(*DEFINITION MODULE SimpleCode;*)

  PROCEDURE Emit2GivePC(VAR fPC: INTEGER); 
    (* if code is moved no trackkeeping is done *)

  PROCEDURE Fixup2(fPC: INTEGER);
    (* long jump at fPC jumps to PC *)
                                             (* the Emit2GivePC, Fixup2 pair 
                                                is used in the FOR and CASE statements;
                                                no trackkeeping of code moves *)
    
  PROCEDURE EmitPacked(op, f: CARDINAL);  

  PROCEDURE EmitLI(i: CARDINAL);
  PROCEDURE EmitLSW(i: CARDINAL);
  PROCEDURE EmitSSW(i: CARDINAL);
  PROCEDURE EmitCL(i: CARDINAL);

  PROCEDURE UAddToTop(x: CARDINAL);
    (* address arithmetic intended!;
       x positive, no wraparound allowed,
        may generate either LSA or UADD instructions*)
 
  PROCEDURE IAddToTop(x: INTEGER);
    (* generates code to increment the value on top of stack,
       integer arithmetic *)

  PROCEDURE UConstMul(c: CARDINAL);
    (* multiplies top value by c, unsigned arithmetic *)

  PROCEDURE UConstDiv(c: CARDINAL);
    (* divides top element by c, unsigned arithmetic *)

  PROCEDURE UConstMod(c: CARDINAL);
    (* take modulus of top element, unsigned arithmetic *)

(*END SimpleCode;*)


(*MODULE TypeSystem;*)

  PROCEDURE IsArrayType(fsp:Stptr): BOOLEAN;   

  PROCEDURE IsSetType(fsp:Stptr): BOOLEAN;   

  PROCEDURE ByteSize(fsp: Stptr): BOOLEAN;   

(*END TypeSystem;*)


(*DEFINITION MODULE BlockSystem;*)
 
  PROCEDURE GenBlockEntry;
    (* fix proceduretable and generate entry sequence *)

  PROCEDURE GenBlockReturn;
    (* generate a return from a block *)

  PROCEDURE TerminateBlock;
    (* generate exit sequence and 
       terminates handling of the current block*)

  PROCEDURE CodeBody(fnp: Idptr);
    (* inline body of CODE procedure *)

  PROCEDURE GenCodeBlock;
    (* complete non-inline CODE procedure *)

(*END BlockSystem;*)
   
END MCP4CodeSys.
  
  

