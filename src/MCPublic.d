(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCPublic:                         *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCPublic;    (* LG *)

  EXPORT QUALIFIED
    il1File, il2File, ascFile,
    modFile, lstFile, symFile, refFile, objFile,
    Compilerstatus, Statset, compstat,
    comptime, symfileextension;

  VAR
    (* file numbers of files used by the compiler *)
    il1File : CARDINAL; (* interpass file *)
    il2File : CARDINAL; (* interpass file *)
    ascFile : CARDINAL; (* identifier table file *)
    modFile : CARDINAL; (* compiler input file *)
    lstFile : CARDINAL; (* listing file *)
    symFile : CARDINAL; (* symbol file *)
    refFile : CARDINAL; (* reference file *)
    objFile : CARDINAL; (* code file *)

  TYPE
    Compilerstatus = (globerrs, passerrs, symerrs, defs, syms,
                      listings, querys, compiles, finis);
    Statset = SET OF Compilerstatus;

  VAR
    compstat  : Statset; (* status communication in compiler *)

  VAR
    comptime : ARRAY [0..2] OF CARDINAL;
      (* date, minute, millisecond *)

    symfileextension : ARRAY [0..2] OF CHAR; 
      (* filename extension of symbol files *)

END MCPublic.
