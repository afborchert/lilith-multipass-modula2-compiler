DEFINITION MODULE Options; (* AKG 28.05.80 *)
                           (* modified for PC  LG 10.10.80 *)

  EXPORT QUALIFIED FileNameAndOptions, GetOption, Termination;

  TYPE Termination = (normal, empty, can, esc);

  PROCEDURE FileNameAndOptions(default: ARRAY OF CHAR; VAR name: ARRAY OF CHAR;
                               VAR term: Termination; acceptOption: BOOLEAN);

    (*Read file name and options from terminal.
      Reads until a <cr>, blank, <can>, or <esc> is typped.
      To read the file name a default name can be proposed.
      name returns the read file name.
      term returns the status of the input termination:
          normal : normally terminated
          empty  : normally terminated, but name is empty
          can    : <can> is typed, input line cancelled
          esc    : <esc> is typed, no file specified.
      If acceptOption is TRUE, then options preceded by '/' will be accepted.*)

  PROCEDURE GetOption(VAR optStr: ARRAY OF CHAR; VAR length: CARDINAL);

    (*To be called repeatedly after FileNameAndOptions, until length = 0.
      The characters of the next option are returned in optstr
      (terminated with a 0C character if length <= HIGH(optstr) ). *)

END Options.
