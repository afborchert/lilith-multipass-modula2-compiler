(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*     running on Lilith Computer        *
*                                       *
*     MCFileNames:                      *
*                                       *
*     file name part                    *
*     of the Modula-2 compiler          *
*                                       *
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)
 
DEFINITION MODULE MCFileNames;   (* LG *)

  EXPORT QUALIFIED 
    FileName, modName, symName, refName, objName, lstName;

  CONST
    namelength = 32;

  TYPE
    FileName = ARRAY [0 .. namelength - 1] OF CHAR;

  VAR
    modName : FileName;
    symName : FileName;
    refName : FileName;
    objName : FileName;
    lstName : FileName;
    
END MCFileNames.
