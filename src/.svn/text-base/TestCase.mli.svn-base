(**
	TestCases are either standalone or nested functions that make
	verification calls. A TestCase can be run and each atomic 
	TestCase will be run and it's verification results compiled and saved.

	The main advantage of this technique is that both the verifications inside
	code being tested and verifications called by the test code are treated
	equally, and they can be moved into or out of the application code based
	on design requirements, not on whether the unit test framework should pick
	them up.
	*)
	

type t


val createAtomic: label:string -> testFun:(unit -> unit) -> t

(** group a set of testCases into a single one *)
val createSuite: label:string -> children:t list -> t

val getLabel: t -> string

val isAtomic: t -> bool

(* will fail if isAtomic *)
val getChildren: t -> t list

(** Outcomes/Results *)

(** encapsulates the results of a TestCase run in the hierarchy of the original
TestCase. *)
type tOutcomeTree 

type tOutcomePath = string list (** test label path, from root to leaf *)

(** calcs the (numFails, numTotalOutcomes) pair, for all outcomes in a tree *)
val calcVerificationFailRate: tOutcomeTree -> (int * int)

(** calcs the (numFails, numTotalAtoms) pair, for all Atom tests in the tree *)
val calcAtomFailRate: tOutcomeTree -> (int * int)

(** for the given tree, deduce/extract the outcome paths of all atomic tests
that had at least one failure *)
val findFailPaths: tOutcomeTree -> tOutcomePath list


(** {5 Running Tests} *)

(** set to true to get a stack trace when a failure occurs. should normally
be left as false (the default) *)
val setGlobalPrintStackTrace: bool -> unit

(** runs the test. see the logs for the output *)
val run: t -> tOutcomeTree




