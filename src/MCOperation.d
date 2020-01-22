(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCOpreations:                     *
*                                       * 
*     Arithmetic oprations for          *
*     constant expression evaluation    *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCOperations;        (* LG *)

  (* operations on constant expressions in compiler *)
  (* CAUTION: operations are based on 16-bit arithmetic *)
 
  FROM MCBase IMPORT Stptr, Symbol, Constval;

  EXPORT QUALIFIED RelOp, AddOp, MulOp, NotOp;

  PROCEDURE RelOp(c1,c2: Constval; VAR res: Constval; op: Symbol;
                  tp: Stptr; VAR err: BOOLEAN);
    (* evaluation of a relational operation on constant values *)

    (* reals not implemented *)
 
  PROCEDURE AddOp(c1,c2: Constval; VAR res: Constval; op: Symbol;
                  VAR tp: Stptr; VAR err: BOOLEAN); 
    (* evaluation of additional operations on constant values *)
    (* return the result value by res and the result type by tp *)
    (* err indicates an overflow error *) 

    (* reals not implemented *) 

  PROCEDURE MulOp(c1,c2: Constval; VAR res: Constval; op: Symbol; 
                  VAR tp: Stptr; VAR err: BOOLEAN); 
    (* evaluation of multiplicational operations on constant values *)
    (* return the result value by res and the result type by tp *)
    (* err indicates an overflow or zero-division error *) 

    (* reals not implemented *)

  PROCEDURE NotOp(c1: Constval; VAR res: Constval);
    (* evaluation of NOT operation on a constant value *)

END MCOperations.
