(** A global buffer for log messages. All messages go to standard out.

*)


(** Logger.levels indicate what priority a log message has and/or which messages
the logger will log.*)
type tLevel =
	|Crash (** Failure in program leading to exit *)
	|Warn (** Incorrect operation detected but recoverable *)
	|Test (** Special level for test code *)
	|Info (** Info on subsystem start up, tear down, configuration, etc *)
	|Debug (** Info on subroutine calls, input/output values *)
	|Spew (** Info on intermediate states within functions and loops *)

(** {5 Log Message Handlers} *)
val logMessage: tLevel -> string -> unit

val crash: string -> unit
val warn: string -> unit
val test: string -> unit
val info: string -> unit
val debug: string -> unit
val spew: string -> unit

(** {5 Global Settings} *)

(** sets the minimum precedence that future log messages will need to be sent
to the destination. All others will be ignored. The precedence is the order
the levels appear in the the [Log.tLevel] type declaration.  *)
val setRunLevel: tLevel -> unit

(** flushes the message buffer if it exists by converting any internally stored
log messages to strings and printing them on the destination*)
val flushBuffer: unit -> unit
