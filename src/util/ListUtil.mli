
(** create a list by making [length] calls to [genFun]. the last call will be 
the head of the list *)
val init: genFun:(unit -> 'a) -> length: int -> 'a list

(** same as initList but the function gets a counter for how many calls have 
been made. first call gets zero, last call gets [length] - 1 *)
val initIdx: genFun:(int -> 'a) -> length: int -> 'a list

(* first n elements of lst *)
val headN: 'a list -> int -> 'a list

(* last n elements of lst *)
val tailN: 'a list -> int -> 'a list

(** split a list into the first N elements, and all the rest. *)
val splitN: 'a list -> int -> 'a list * 'a list

(** makes a list of every pair of elements from the input list *)
val makePairs: 'a list -> ('a * 'a) list


(** for each element in the list, calls the iterFun with
(iterFun element's_index element) *)
val forEachIdx: 'a list -> iterFun:(int -> 'a -> unit) -> unit

(** performs a mapping from one list to another, where the map function
takes both the index of the original element and it's value (head of the
list is zero). *)
val mapIdx: 'a list -> mapFun:(int -> 'a -> 'b) -> 'b list

(** takes an list and a toString method for the element type. returns
a string of the form "[elem1; elem2, ... elemN]" *)
val toString: 'a list -> ('a -> string) -> string

(** compares two lists using a compare() method for the elements. works the
same as Iterator.compare *)
val compare: 'a list -> 'a list -> ('a -> 'a -> int) -> int

(** uses toString and compare methods above *)
val makeComparer: 'a Comparer.t -> 'a list Comparer.t

val sort: 'a list -> 'a Comparer.t -> 'a list

(** fails if list is empty*)
val findMin: 'a list -> 'a Comparer.t -> 'a

(** fails if list is empty*)
val findMax: 'a list -> 'a Comparer.t -> 'a 

(** for a list of lists of the form:
	[[a0; a1; ...aN]; [b0; b1;...bM];[c0;c1...cP]...]
	generates all cross prodcuts of the type:
	[[a0;b0;c0]; [a0;b0;c1];[a0;b0;c2]....] 
*)
val makeCrossProduct: 'a list list -> 'a list list

(** similar to makePairs, but uses two different lists. makes a pair for
every combination of a and b*)
val makeCrossProduct2: 'a list -> 'b list -> ('a * 'b)  list
val makeCrossProduct3: 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val makeCrossProduct4: 'a list -> 'b list -> 'c list -> 'd list ->
	('a * 'b * 'c * 'd) list

val makeCrossProduct5: 'a list -> 'b list -> 'c list -> 'd list -> 'e list ->
	('a * 'b * 'c * 'd * 'e) list

val makeCrossProduct6: 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 
	'f list -> ('a * 'b * 'c * 'd * 'e * 'f) list

val makeCrossProduct7: 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 
	'f list ->  'g list -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) list

val makeCrossProduct8: 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 
	'f list ->  'g list -> 'h list -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list

(** makes a list than has the given length by appending the input list to itself
until it is long enough. *)
val fleshOut: 'a list -> int -> 'a list

(** combines list1 and list2 into a list of pairs. if one list is longer than
the other, uses elems from the head of shorter list again. Therefore the list
of pairs is always the length of the longer input list. *)
val fleshOutCombine2: 'a list -> 'b list -> ('a * 'b) list

val fleshOutCombine3: 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val fleshOutCombine4: 'a list -> 'b list -> 'c list -> 'd list ->
	('a * 'b * 'c * 'd) list

val fleshOutCombine5: 'a list -> 'b list -> 'c list -> 'd list -> 'e list ->
	('a * 'b * 'c * 'd * 'e) list

val fleshOutCombine6: 'a list -> 'b list -> 'c list -> 'd list -> 'e list ->
	'f list -> ('a * 'b * 'c * 'd * 'e * 'f) list

val fleshOutCombine7:  'a list -> 'b list -> 'c list -> 'd list -> 'e list ->
	'f list -> 'g list -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) list

val fleshOutCombine8: 'a list -> 'b list -> 'c list -> 'd list -> 'e list ->
	'f list -> 'g list -> 'h list -> 
	('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list


