(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for Lilith         *
*                                       *
*                                       *
*     MCBase:                           *
*                                       * 
*     private part of the common base   *
*     of the Modula-2 compiler          *
*                                       * 
*     Version C18 of 17.02.81           *
*                                       *
*     Institut fuer Informatik          *
*     ETH-Zuerich                       *
*     CH-8092 Zuerich                   *
*                                       *
****************************************)

DEFINITION MODULE MCBase;     (* LG / CHJ *)

  EXPORT QUALIFIED
    levmax, modnamlength,
    maxprio, noprio,
    Idptr, Stptr, Listptr, Stringptr,
    Idclass, Idset, Structform, Stset,
    Recpart, Varkind, Kindvar, Stpures, Stfuncs,
    Spellix, Levrange, Constval, Modnamarr, Keyarr,
    boolptr, charptr, intptr, cardptr, intcarptr, realptr, 
    procptr, bitsetptr, wordptr, addrptr, processptr,
    substptr, strptrs, root, mainmodp, sysmodp,
    globvarnext, procnumber, stringcount, stringroot,
    Symbol;

  CONST
    levmax = 15;          (* maximal nesting level *)
    modnamlength = 24;    (* maximal length for module identifiers *)
    maxprio = 15;         (* maximal priority allowed for Lilith *)
    noprio = maxprio + 1; (* priority value for no priority *)

  TYPE
    Idptr      = POINTER TO Identrec;
    Stptr      = POINTER TO Structrec; 

    Structform = (enums,bools,chars,ints,cards,words,subranges,reals,
                  pointers,sets,proctypes,arrays,records,hides,opens);
    Stset      = SET OF Structform; 
    Idclass    = (consts, types, vars, fields, pures, funcs,
                  mods, unknown, indrct); 
    Idset      = SET OF Idclass;
    Recpart    = (fixedpart, tagfield, variantpart);

    Structrec  = RECORD
                   size: CARDINAL; (* word size *)
                   stidp: Idptr; (* identifier defining this structure *)
                   inlist: BOOLEAN; (* structure entered into a list *)
                   CASE form: Structform OF 
                     bools,chars,ints,cards,words,reals: (* no field *) 
                    |enums: fcstp: Idptr; cstnr: CARDINAL
                    |subranges: scalp: Stptr; min, max: CARDINAL 
                    |pointers: elemp: Stptr 
                    |sets: basep: Stptr 
                    |arrays: 
                       elp, ixp: Stptr; 
                       dyn: BOOLEAN;
                    |records: 
                       CASE rpart: Recpart OF 
                         fixedpart: fieldp: Idptr; tagp: Stptr
                        |tagfield: fstvarp, elsevarp: Stptr; tagtyp: Stptr
                        |variantpart: nxtvarp, subtagp: Stptr; varval: CARDINAL
                         END (* case Recpart *) 
                    |proctypes:
                       fstparam : Idptr; (* pointer to parameter list *)
                       CASE rkind: Idclass OF 
                         funcs : funcp : Stptr; (* pointer to function type *)
                        |pures : (* no further fields *) 
                       END; 
                    |hides,opens: (* conversion from hides to opens *)
                       openstruc : Stptr; (* used for opens *)
                   END (* case Structform *)
                 END (**record**);

    Varkind    = (noparam, valparam, varparam, copyparam);
    Kindvar    = (global, local, absolute, separate); 
    Spellix    = CARDINAL; 
    Listptr    = POINTER TO Listrec;
    Listrec    = RECORD
                   element : Idptr;
                   next : Listptr;
                 END;
    Stringptr  = POINTER TO Stringval;
    Stringval  = RECORD
                   loadoffset : CARDINAL;
                   valentry : CARDINAL;
                   slink : Stringptr;
                   (* loadoffset means the offset in the     *)
                   (* string data area of the loaded program *)
                   (* valentry is an address to the heap     *)
                   (* entry of the string value              *)
                   (* the structure of valentry should be:   *)
                   (*   POINTER TO ARRAY [0..length] OF CHAR *)
                   (* the element with index length is a 0C  *)
                   (* slink links the strings to be loaded   *)
                 END;
    Constval   = RECORD
                   CASE Structform OF
                     arrays: (* for string constants only *)
                       svalue : Stringptr;
                    |reals: rvalue : POINTER TO REAL;
                   ELSE (* oneword constants *)
                     value : CARDINAL;
                   END;
                 END;
    Keyarr     = ARRAY [0..2] OF CARDINAL;
    Modnamarr  = ARRAY [0..modnamlength-1] OF CHAR;
    Levrange   = [0..levmax]; 
    Stpures    = (decp, disp, exlp, getp, halp, incp, inlp, newp,
                  nprp, putp, trsp);
    Stfuncs    = (absf, adrf, capf, chrf, fltf, higf, oddf, ordf,
                  sizf, trcf, tszf, valf);

    Identrec   = RECORD 
                   name: Spellix;
                   link: Idptr; 
                   CASE BOOLEAN OF
                     FALSE: nxtidp: Idptr
                    |TRUE : idtyp : Stptr
                   END (* case BOOLEAN *);
                   globmodp: Idptr; (* pointer to global module *)
                   CASE klass: Idclass OF 
                     types: (* no further fields *)
                    |consts,unknown: (* unknown may convert to consts *)
                       cvalue: Constval; 
                    |vars: 
                       indaccess : BOOLEAN; (* indirect access to value *)
                       state : Kindvar;
                       vkind : Varkind;
                       vlevel : Levrange;
                       vaddr : CARDINAL; (* offset *) 
                       vlink : Idptr;     (* variables or parameters *)
                    |fields: fldaddr: CARDINAL  
                    |pures, funcs, mods: 
                       CASE isstandard: BOOLEAN OF
                         TRUE:
                           CASE Idclass OF
                             pures: pname: Stpures 
                            |funcs: fname: Stfuncs 
                           END (* case Idclass *) 
                        |FALSE: 
                           procnum : CARDINAL;
                           locp : Idptr;
                           msp : Listptr; 
                           plev : Levrange;
                           varlength : CARDINAL;
                           priolev : CARDINAL;
                           externalaccess : BOOLEAN;
                           CASE Idclass OF
                             pures,funcs:
                               CASE codeproc : BOOLEAN OF
                                 FALSE:
                                   locvarp : Idptr; (* local variables, no parameters *)
                                |TRUE:
                                   codelength : CARDINAL;
                                   codeentry : CARDINAL;
                                   (* codeentry is a heap entry of code values *)
                                   (* belonging to the code procedure          *)
                                   (* it represents the following structure:   *)
                                   (*   POINTER TO                             *)
                                   (*     ARRAY [1..codelength] OF CARDINAL    *)
                               END; (* CASE *)
                            |mods:
                               impp: Listptr;
                               expp: Idptr;
                               qualexp: BOOLEAN;
                               CASE globalmodule : BOOLEAN OF
                                 FALSE: (* no further field *)
                                |TRUE:
                                   globvarp : Idptr; (* global variables *)
                                   modnum : CARDINAL;
                                   modulekey : Keyarr;
                                   identifier : Modnamarr;
                               END; (* case globalmodule *)
                           END; (* case Idclass *)
                       END (* case isstandard *);
                    |indrct: (* no further fields *) 
                   END (* case Idclass *) 
                 END (**record**);

  VAR 
    boolptr    : Stptr;   (* structure of type BOOLEAN *) 
    charptr    : Stptr;   (* structure of type CHAR    *) 
    intptr     : Stptr;   (* structure of type INTEGER *) 
    cardptr    : Stptr;   (* structure of type CARDINAL *)
    intcarptr  : Stptr;   (* structure of intcar - type *)
    realptr    : Stptr;   (* structure of type REAL *)
    procptr    : Stptr;   (* structure of type PROC *)  
    bitsetptr  : Stptr;   (* structure of type BITSET  *) 
    wordptr    : Stptr;   (* structure of type WORD    *) 
    addrptr    : Stptr;   (* structure of type ADDRESS *) 
    processptr : Stptr;   (* structure of type PROCESS *) 
    strptrs    : ARRAY [0 .. 20] OF Stptr;
                          (* table to string-structure entries *)
    substptr   : Idptr;   (* list of procedures to be substituted *)
    root       : Idptr;   (* root of standardname entries  *) 
    mainmodp   : Idptr;   (* pointer to main module *)  
    sysmodp    : Idptr;   (* pointer to module SYSTEM *)
    globvarnext: CARDINAL;(* next address for global variables *)
    procnumber : CARDINAL;(* number of procedures in program *)
    stringcount: CARDINAL;(* number of bytes needed for string area *)
    stringroot : Stringptr;(* chain of strings to be loaded *)

  TYPE Symbol=(eop,                                                      (*   0B *) 
  (* pass 1 *) andsy,divsy,times,slash,modsy,notsy,plus,minus,orsy,      (*  11B *) 
               eql,neq,grt,geq,lss,leq,insy,                             (*  20B *)
               lparent,rparent,lbrack,rbrack,lconbr,rconbr,              (*  26B *)
               comma,semicolon,period,colon,range,                       (*  33B *)
               constsy,typesy,varsy,arraysy,recordsy,variant,setsy,      (*  42B *) 
               pointersy,tosy,arrow,hidden,                              (*  46B *)
               importsy,exportsy,fromsy,qualifiedsy,                     (*  52B *)
               codesy,beginsy,                                           (*  54B *)
               casesy,ofsy,ifsy,thensy,elsifsy,elsesy,loopsy,            (*  63B *)
               exitsy,repeatsy,untilsy,whilesy,dosy,withsy,              (*  71B *)
               forsy,bysy,returnsy,becomes,endsy,                        (*  76B *)
               call,endblock,                                            (* 100B *)
               definitionsy,implementationsy,proceduresy,modulesy,       (* 104B *)
               symbolsy,                                                 (* 105B *)
               ident,intcon,cardcon,intcarcon,realcon,charcon,stringcon, (* 114B *)
               option,errorsy,eol,                                       (* 117B *)
  (* pass 2 *) namesy,                                                   (* 120B *)
  (* pass 3 *) field,anycon);                                            (* 122B *)

END MCBase.
