DEFINITION MODULE MCP4ExpressionSys; (* Ch. Jacobi  5.4.81 *)

(* expressions 
   for Pass 4 of the Compiler for the Lilith Computer *)

  FROM MCP4AttributSys IMPORT Attribut;
  EXPORT QUALIFIED 
    EnterWith, ExitWith, 
    Designator, Expression, ExpressionAndLoad;

  PROCEDURE EnterWith(VAR fat: Attribut); 
    (* Store fat for further local use;  
       code for entry of a with statement *)

  PROCEDURE ExitWith;
    (* Exit the innermost with statement *)

  PROCEDURE Designator(VAR fat: Attribut);   

  PROCEDURE Expression(VAR fat: Attribut);          
    (* fat describes the first operand and the result *)

  PROCEDURE ExpressionAndLoad;

END MCP4ExpressionSys.

