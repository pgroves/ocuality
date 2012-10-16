(**
	a singleton that logs and tracks success/failure declarations.
*)

(** {5 Declarations} *)
val declareSuccess: label: string -> unit

val declareFailure: label: string -> unit


(** {5 History Tracking} *)
(** the label of the verification and whether it succeeded/failed *)
type tOutcome = 
	|Success of string 
	|Failure of string 
	|IgnoredFailure of string

(** set to 'true' to collect the status history. if false, getStatusHistory
will fail. if false, lastStatus will still work. *)
val setCollectOutcomes: bool -> unit

(** get the result of the most recent call to a [doVerify] method *)
val getLastOutcome: unit -> tOutcome

(** gets the list of Failure outcomes since the last call to [clearHistory] *)
val getFailureHistory: unit -> tOutcome list

(** get the total number of outcomes since the last call to [clearHistory] *)
val getHistoryCount: unit -> int

(** resets the outcomeHistory to an empty list *)
val clearHistory: unit -> unit

(** {5 Response Management} *)

type tResponse = 
	|Continue (** continue when there is a failure *)
	|Exit (** raise an exception that exits the program when there is a failure *)

(** set the global response to failed verification. default is Exit. also set
the level the log messages for a failure will be set to on failure, default
is Log.Crash *)
val setFailResponse: tResponse -> Log.tLevel -> unit

(* sets the log level of successful verify commands. default is Log.Spew *)
val setSuccessResponse: Log.tLevel -> unit

(** enters a mode where failures are logged as IgnoredFailures, which are
treated as Success's and logged as \[IgnoredFailure\]'s *)
val startNoFail: unit -> unit

(** exits the ignore failures mode. returns true if at least one failure occured
since the call to startNoFail entered the mode. *)
val stopNoFail: unit -> bool
