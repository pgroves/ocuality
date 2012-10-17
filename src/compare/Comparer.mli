(**
    Abstraction for a comparison function and to_string method.
    

*)


type 'a t

(** basic constructor *)
val create: 
	compareFun: ('a -> 'a -> int) ->
	toStringFun: ('a -> string) ->
	'a t

(** use an existing comparar, but override one or both of the methods. *)
val makeWith: 'a t ->
	compareFun: ('a -> 'a -> int) option ->
	toStringFun: ('a -> string) option ->
	'a t

(** calls the Comparer.t's inner toString method *)
val toString: 'a t -> 'a -> string

(** calls the Comparer.t's inner compare method *)
val compare: 'a t -> 'a -> 'a -> int

(** {5 Ordering Comparisons} *)
(** based on the returned value of compare() *)


val lessThan: 'a t -> 'a -> 'a -> bool
val lt: 'a t -> 'a -> 'a -> bool

val lessThanOrEqual: 'a t -> 'a -> 'a -> bool
val lte: 'a t -> 'a -> 'a -> bool

val greaterThan: 'a t -> 'a -> 'a -> bool
val gt: 'a t -> 'a -> 'a -> bool

val greaterThanOrEqual: 'a t -> 'a -> 'a -> bool
val gte: 'a t -> 'a -> 'a -> bool


val areEqual: 'a t -> 'a -> 'a -> bool
val notEqual: 'a t -> 'a -> 'a -> bool

(** {5 Ranges} *)

(** true if x is in the range [min, max) *)
val inRange: 'a t -> x:'a -> min:'a -> max:'a -> bool

(** {5 Selections} *)

val theGreaterOf: 'a t -> 'a -> 'a -> 'a

val theLessorOf: 'a t -> 'a -> 'a -> 'a

(** {5 Ocaml Types} *)
(** prebuilt comparers for builtin Ocaml types. These use Pervasives.compare *)

val ints: int t
val floats: float t
val strings: string t

val nums: Num.num t

(** make a comparer that has 'delta' slack in it's comparisons. two floats are
considered equal if they are within 'delta' of each other, and must be more
than delta apart to be gt or lt  *)
val makeFloatsDelta: delta:float -> float t

(** {5 Composition of Higher Order Comparer.t's *)


(** make a comparer for a pair of values, using the comparers for the existing
type *)
val makePair: 'a t -> 'b t -> ('a * 'b) t

val makeTriple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val makeQuad: 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** make a Comparer.t for elements of type ('a option). The resulting compare()
function considers Some(_) > None, and the to String function returns
"None" or "Some(x)" *)
val makeOption: 'a t -> 'a option t

(** make a comparer for type 'b when there exists a one-to-one mapping function
to 'a and a 'a comparer. *)
val map: 'a t -> ('b -> 'a) -> 'b t

(** creates a new comparer with a comparison function that is opposite of
the input comparer. E.g. if you want a comparer that says an element of better
(lower) 'rank' is greater than one with a worse rank, use this to invert 
Comparer.ints. *)
val invert: 'a t -> 'a t
