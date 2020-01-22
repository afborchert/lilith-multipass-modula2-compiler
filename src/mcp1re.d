(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCP1Reals:                        *
*                                       * 
*     Real constant handling in Pass 1  *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCP1Reals;         (* LG *)

  FROM MCBase IMPORT Constval;

  EXPORT QUALIFIED
    InitRealConst, ConvertToFraction, ConvertToExponent, TermRealConst;

  PROCEDURE InitRealConst;
    (* initialise the calculation of a constant real number *)

  PROCEDURE ConvertToFraction(ch: CHAR);
    (* convert a character to the fraction of a constant real number *)

  PROCEDURE ConvertToExponent(ch: CHAR);
    (* convert a character to the exponent of a constant real number *)

  PROCEDURE TermRealConst(VAR cval: Constval; VAR err: BOOLEAN);
    (* terminate the calculation of a constant real number *)

END MCP1Reals.
