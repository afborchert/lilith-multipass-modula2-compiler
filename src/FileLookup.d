DEFINITION MODULE FileLookup;        (* LG 26.02.81 *)

  (* lookup on files with different searching strategies *)

  EXPORT QUALIFIED LookupFile;

  PROCEDURE LookupFile(name: ARRAY OF CHAR;
                       defaultextension: ARRAY OF CHAR;
                       VAR filenum: CARDINAL;
                       query, acceptdefault, acceptoptions: BOOLEAN;
                       VAR acceptedfilename: ARRAY OF CHAR;
                       VAR goodfile: BOOLEAN);

    (* for implementation the modules FileNames, *)
    (* Options and FilePool are imported         *)

    (* name          : the name is displayed and a default *)
    (*                 file name is constructed from name *)
    (* filenum       : file number is assigned from FilePool *)
    (* query         : explicit asking for file name *)
    (* acceptdefault : accept a default file name *)
    (* acceptoptions : accept options appended to file name *)
    (*                 options are not evaluated *)
    (* goodfile      : lookup was successfull *)

END FileLookup.
