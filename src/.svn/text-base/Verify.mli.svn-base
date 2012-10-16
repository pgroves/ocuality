(**
	Verify is used to make runtime checks of predicates and log the results.
	Verify can either continue or terminate the program in the event of
	a failed verification.

	This Verify module makes declarations with CheckHandler
*)



(** {5 Check Calls} *)

(** simplest type. the predicate is successful if true. *)
val predicate: label:string -> pred: bool -> unit

(** force a success condition with the input label *)
val success: label: string -> unit

(** force a failure condition with the input label *)
val failure: label: string -> unit

(** verifies that when the [block] method is evaluated, it produces at
least one failure *)
val doesFail: label:string -> block:(unit -> unit) -> unit


(** force a failure condition and then actually raise a Failure exception.
for situations where the program has no way to continue and a return value
of 'a is needed to compile *)
val crash: label: string -> 'a

(** executes the block, if any exception is raised it will be caught and
a [crash] will be called with the higher level label *)
val crashOnException: label:string -> block:(unit -> 'a) -> 'a

(** {6 Compareable Values} *)

val gt: label:string ->  cmp:'a Comparer.t -> x1: 'a -> x2: 'a -> unit

val gte: label:string -> cmp:'a Comparer.t -> x1: 'a -> x2: 'a -> unit

val lt: label:string -> cmp:'a Comparer.t -> x1: 'a -> x2: 'a -> unit

val lte: label:string ->  cmp:'a Comparer.t  -> x1: 'a -> x2: 'a -> unit

val areEqual: label:string -> cmp:'a Comparer.t -> x1: 'a -> x2: 'a -> unit

val notEqual: label:string -> cmp:'a Comparer.t -> x1: 'a -> x2: 'a -> unit

val inRange: label:string-> cmp:'a Comparer.t -> x: 'a -> min: 'a -> max: 'a -> unit

(** {6 Lists} *)

(** check the length of a list against a verification of an int. in the
input [intVerifyFun], x1 will map to x1 and the length of list x2 will map
to x2 *)
val listLength: 
	label: string -> 
	intVerifyFun: (label:string -> x1: int -> x2: int -> unit)-> 
	x1: int ->
	x2: 'a list -> 
	unit

(** verifies the tail of the list x is equal to the list endOfX *)
val listEndsWith: 
		label: string -> 
		?printFun:('a -> string) -> 
		x: 'a list -> 
		endOfX: 'a list -> 
		unit
(** {6 Code Status} *)

val notImplemented: moduleName:string -> methodName:string -> 'a

(** similar to notImplemented, but doesn't cause a 'crash' condition. 
intended for use when a verify* method is not implemented, but you don't
want that to cause an error, just a log message. *)
val notVerified: moduleName:string -> methodName:string -> unit
