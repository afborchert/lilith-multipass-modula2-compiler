DEFINITION MODULE Program; (* provisorisch A. Borchert *)
 EXPORT QUALIFIED Call, Status;

 TYPE Status = (normal, failed); (* weitere koennen folgen *)

 PROCEDURE Call(filename: ARRAY OF CHAR; flag: BOOLEAN; VAR stat: Status);

END Program.
