DEFINITION MODULE MCP4CallSys;  (* Ch. Jacobi  5.4.81 *)

(* procedure calls 
   for Pass 4 of the Compiler for the Lilith Computer *)

  FROM MCP4AttributSys IMPORT Attribut;
  EXPORT QUALIFIED ProcFuncCall;
 
  PROCEDURE ProcFuncCall(VAR fat: Attribut);   

END MCP4CallSys.



