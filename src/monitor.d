DEFINITION MODULE Monitor; (* A. Borchert *)

 EXPORT QUALIFIED Time, GetTime;

 TYPE
   Time = RECORD
          day : CARDINAL;
          minute : CARDINAL;
          millisecond : CARDINAL
          END;

 PROCEDURE GetTime(VAR time: Time);

END Monitor.
