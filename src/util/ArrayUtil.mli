
(** modify an array in place, where each element is replaced by the result
of [modFunction index element] *)
val modArray: modFun:(int -> 'a -> 'a) -> arr: 'a array -> unit

(** counts how many times the element exists (isEqual) in the array *)
val countInstancesInArray: 'a array -> 'a -> int

(** takes an array and a toString method for the element type. returns
a string of the form "[|elem1; elem2, ... elemN|]" *)
val toString: 'a array -> ('a -> string) -> string

(** compares two arrays using a compare() method for the elements. works the
same as Iterator.compare *)
val compare: 'a array -> 'a array -> ('a -> 'a -> int) -> int

(** uses toString and compare methods above *)
val makeComparer: 'a Comparer.t -> 'a array Comparer.t


(** in place sort *)
val sort: 'a array -> 'a Comparer.t -> unit

(** sorts a copy of the input array. does not affect the input array *)
val sortCopy: 'a array -> 'a Comparer.t ->  'a array

