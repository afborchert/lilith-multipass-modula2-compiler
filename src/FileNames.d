(***************************************
*                                      *
*          M E D O S - 2               *
*          *************               *
*                                      *
*                                      *
*          FileNames:                  *
*                                      *
*          FileNames allows to read    *
*          a filename according to     *
*          a default specification.    * 
*                                      *
*          Version 2  11.10.80         *    
*                                      *
*                                      *
*          Svend Erik Knudsen          *
*          Institut fuer Informatik    *
*          ETH-Zuerich                 *
*          CH-8092 Zuerich             *
*                                      *
***************************************)

DEFINITION MODULE FileNames;

  EXPORT QUALIFIED
    ReadFileName, Identifiers, IdentifierPosition;


  PROCEDURE ReadFileName(VAR fn: ARRAY OF CHAR; dfn: ARRAY OF CHAR);
    (* ReadFileName reads the filename fn according to a defaultfile-
       name dfn. If no valid name could be returned, fn[0] is set to
       0C. *)

  PROCEDURE Identifiers(fn: ARRAY OF CHAR): CARDINAL;
    (* Identifiers returns the number of identifiers in the filename
       fn. *)

  PROCEDURE IdentifierPosition(fn: ARRAY OF CHAR; identno: CARDINAL): CARDINAL;
    (* IdentifierPosition returns the index of the first character of
       the identifier 'identno' in filename fn. The first identifier
       in the filename is given number 0. *)


END FileNames.

(*
    Syntax of the different names:

    filename        = mediumname [ "." name ] [ 0C | ' ' ] .
    mediumname      = ident .
    name            = [ qualident "." ] lastident .
    qualident       = ident { "." ident } .
    lastident       = ident .
    ident           = letter { letter | digit } .

    defaultfilename = mediumname [ "." [ defaultname ] ] [ 0C | ' ' ] .
    defaultname     = [ [ qualident ] "." ] lastident .

    inputfilename   = [ "#" [ mediumname ] [ "." inputname ] | inputname ] .
    inputname       = defaultname .


    The scanning of "inputfilename" is terminated by the characters
    <esc> and <can> or at a syntatically correct position by the
    caracters <eol>, " " and "/". The termination character can be
    read after the call. For correction of typing errors, <del> is
    accepted at any place in the input.
                                                                   *)

