DEFINITION MODULE ProgramMessage; (* A. Borchert *)
 FROM Program IMPORT Status;
 EXPORT QUALIFIED WriteStatus;

 PROCEDURE WriteStatus(exerror: Status);

END ProgramMessage.
