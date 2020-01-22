(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCSymFileDefs:                    *
*                                       * 
*     definition of symbols             *
*     used on symbol files              *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCSymFileDefs;  (* LG *)

  EXPORT QUALIFIED
    symFileKey, SymFileSymbols;

  CONST
    symFileKey = 105B; (* identification of symbol file version *)

  TYPE
    SymFileSymbols =
      (endfileSS,
       unitSS, endunitSS,
       importSS, exportSS,
       constSS, normalconstSS, realconstSS, stringconstSS,
       typSS, arraytypSS, recordtypSS, settypSS, pointertypSS, hiddentypSS,
       varSS,
       procSS, funcSS,
       identSS,
       periodSS, colonSS, rangeSS,
       lparentSS, rparentSS,
       lbracketSS, rbracketSS,
       caseSS, ofSS, elseSS, endSS);

END MCSymFileDefs.
