DEFINITION MODULE MCMnemonics;          (* Ch. Jacobi *)
                                          (* last modification 14.2.81 *)
(* mnemonics for the instructions of the Lilith computer
   for Pass 4 of the Compiler *)

  EXPORT QUALIFIED                                                  
    LIn,
    LIB,   LIW,   LID,   LLA,   LGA,   LSA,  LEA,    
    JPC,   JP,    JPFC,  JPF,   JPBC,  JPB,  ORJP, ANDJP,

    LLW,   LLD,   LEW,   LED,   LLWn,
    SLW,   SLD,   SEW,   SED,   SLWn,

    LGW,   LGD,   LGWn,
    SGW,   SGD,   SGWn,

    LSWn,
    SSWn,

    LSW,   LSD,   LSD0,  LXFW,  LSTA,  LXB,  LXW,  LXD,
    DADD,  DSUB,  DMUL,  DDIV,  DSHL,  DSHR,
    SSW,   SSD,   SSD0,  SXFW,  TS,    SXB,  SXW,  SXD,
    FADD,  FSUB,  FMUL,  FDIV,  FCMP,  FABS, FNEG, FFCT,

    READ,  WRITE, DSKR,  DSKW,  SETRK, UCHK, ESC,  SYS,           
    ENTP,  EXP,   ULSS,  ULEQ,  UGTR,  UGEQ, TRA,  RDS,
    LODFW, LODFD, STORE, STOFV, STOT,  COPT, DECS, PCOP,
    UADD,  USUB,  UMUL,  UDIV,  UMOD,  ROR,  SHL,  SHR,

    FOR1,  FOR2,  ENTC,  EXC,   TRAP,  CHK,  CHKZ, CHKS,  
    EQL,   NEQ,   LSS,   LEQ,   GTR,   GEQ,  ABSS, NEG,
    ORR,   XORR,  ANDD,  COM,   INN,   LIN,  MSK,  NOTT,
    ADD,   SUB,   MUL,   DIVV,  BIT,   NOP,  MOVF,  
     
    MOV,   CMP,   DDT,   REPL,  BBLT,  DCH,  UNPK, PACK,           
    GB,    GB1,   ALOC,  ENTR,  RTN,   CX,   CI,   CF,
    CL,    CLn;

  CONST
    LIn  =   0B;  (* LI0..LI15 *)
    LIB  =  20B;                LIW  =  22B;  LID  =  23B;  
    LLA  =  24B;  LGA  =  25B;  LSA  =  26B;  LEA  =  27B;
    JPC  =  30B;  JP   =  31B;  JPFC =  32B;  JPF  =  33B;
    JPBC =  34B;  JPB  =  35B;  ORJP =  36B;  ANDJP=  37B;  
            
    LLW  =  40B;  LLD  =  41B;  LEW  =  42B;  LED  =  43B;
    LLWn =  40B;  (* LLW4..LLW15 *)
    SLW  =  60B;  SLD  =  61B;  SEW  =  62B;  SED  =  63B;
    SLWn =  60B;  (* SLW4..SLW15 *)

    LGW  = 100B;  LGD  = 101B;
    LGWn = 100B;  (* LGW2..LGW15 *)
    SGW  = 120B;  SGD  = 121B;
    SGWn = 120B;  (* SGW2..SGW15 *)   

    LSWn = 140B;  (* LSW0..LSW15 *)
    SSWn = 160B;  (* SSW0..SSW15 *)

    LSW  = 200B;  LSD  = 201B;  LSD0 = 202B;  LXFW = 203B;            
    LSTA = 204B;  LXB  = 205B;  LXW  = 206B;  LXD  = 207B;
    DADD = 210B;  DSUB = 211B;  DMUL = 212B;  DDIV = 213B;
                                DSHL = 216B;  DSHR = 217B;
    SSW  = 220B;  SSD  = 221B;  SSD0 = 222B;  SXFW = 223B;             
    TS   = 224B;  SXB  = 225B;  SXW  = 226B;  SXD  = 227B;
    FADD = 230B;  FSUB = 231B;  FMUL = 232B;  FDIV = 233B;
    FCMP = 234B;  FABS = 235B;  FNEG = 236B;  FFCT = 237B; 
                     
    READ = 240B;  WRITE= 241B;  DSKR = 242B;  DSKW = 243B;
    SETRK= 244B;  UCHK = 245B;  ESC  = 246B;  SYS  = 247B;
    ENTP = 250B;  EXP  = 251B;  ULSS = 252B;  ULEQ = 253B;
    UGTR = 254B;  UGEQ = 255B;  TRA  = 256B;  RDS  = 257B;
    LODFW= 260B;  LODFD= 261B;  STORE= 262B;  STOFV= 263B;
    STOT = 264B;  COPT = 265B;  DECS = 266B;  PCOP = 267B;
    UADD = 270B;  USUB = 271B;  UMUL = 272B;  UDIV = 273B;
    UMOD = 274B;  ROR  = 275B;  SHL  = 276B;  SHR  = 277B;

    FOR1 = 300B;  FOR2 = 301B;  ENTC = 302B;  EXC  = 303B;
    TRAP = 304B;  CHK  = 305B;  CHKZ = 306B;  CHKS = 307B;
    EQL  = 310B;  NEQ  = 311B;  LSS  = 312B;  LEQ  = 313B;
    GTR  = 314B;  GEQ  = 315B;  ABSS = 316B;  NEG  = 317B;
    ORR  = 320B;  XORR = 321B;  ANDD = 322B;  COM  = 323B;
    INN  = 324B;  LIN  = 325B;  MSK  = 326B;  NOTT = 327B;
    ADD  = 330B;  SUB  = 331B;  MUL  = 332B;  DIVV = 333B;
                  BIT  = 335B;  NOP  = 336B;  MOVF = 337B;  
           
    MOV  = 340B;  CMP  = 341B;  DDT  = 342B;  REPL = 343B;
    BBLT = 344B;  DCH  = 345B;  UNPK = 346B;  PACK = 347B;                          
    GB   = 350B;  GB1  = 351B;  ALOC = 352B;  ENTR = 353B;
    RTN  = 354B;  CX   = 355B;  CI   = 356B;  CF   = 357B;
    CL   = 360B;
    CLn  = 360B;  (* CL1..CL15 *)
END MCMnemonics.

