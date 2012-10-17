(**
	An Iterator over an *ordered* set
*)
type 'a t


val create:
	hasNextFun:(unit -> bool) ->
	nextFun:(unit -> 'a) ->
	'a t

val createFixedSize:
	size: int ->
	nextFun: (unit -> 'a) ->
	'a t

(** like fixedSize, but the next fun gets the index in range [0, size) *)
val createIndexed:
	size: int ->
	nextFun: (int -> 'a) ->
	'a t

val hasNext: 'a t -> bool

val next: 'a t -> 'a

val fromList: 'a list -> 'a t

(** same as fromList *)
val fromArray: 'a array -> 'a t

(** see Hashtbl.iter to understand what elements are returned *)
val fromHashtbl: ('a, 'b) Hashtbl.t -> ('a * 'b)  t 

val fromQueue: 'a Queue.t -> 'a t

(** calls next until no more elements.  *)
val toList: 'a t -> 'a list

(** create a new iterator that behaves like an old one except that a function
is applied to the old one's output. *)
val map: 'a t -> ('a -> 'b) -> 'b t

val map2: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t

val mapIdx: 'a t -> (int -> 'a -> 'b) -> 'b t

(** create a new iterator that pops an element off the old iter, expands it
to a list using the map-to-list function, and then returns the first element
from the list. on the next call it returns the next element from that list. when
it runs out it pops the next elem from the old list and repeats. *)
val expand: 'a t -> ('a -> 'b list) -> 'b t

(** feeds the elements of self into a function that returns an element option.
The source elements will be fed into the function until a Some(elementB) is 
returned, and that is the next value of the new iterator. The function may
return a 'Some' as infrequently as it wants, thereby compressing the number 
elements returned. TODO: should this just take in a ('a option Iterator.t)?*)
val compress: 'a t -> ('a -> 'b option) -> 'b t

(** returns a new iterator that will only return those elements that 
satisfy an acceptance test. A single call to the returned iterator's next()
function will call next() on the sourceIterator until an accepted element
is found. *)
val filter: 'a t -> ('a -> bool) -> 'a t

(** apply a function to each element returned by an iterator *)
val forEach: 'a t -> ('a -> unit) -> unit

(** apply a function to each element returned by an iterator. the function
is also passed the index of the element using a counter that starts at 
zero *)
val forEachIdx: 'a t -> (int -> 'a -> unit) -> unit

(* for each value of the first iterator, will get the next element of
each and apply the function. the second iterator may produce more elements
than the first (extras won't be visited), but the second may not produce fewer *)
val forEach2: 'a t -> 'b t -> ('a -> 'b -> unit) -> unit 

(** same as forEach2, except the function to apply is also given the
value of a counter that starts at zero for the first pair of elements *)
val forEachIdx2: 'a t -> 'b t -> (int -> 'a -> 'b -> unit) -> unit 

(** true if at least one element returns true. stops processing at first 'true'*)
val exists: 'a t -> ('a -> bool) -> bool

(** true if at least one pair returns true. stops processing at first 'true'*)
val exists2: 'a t -> 'b t -> ('a -> 'b -> bool) -> bool

val existsIdx: 'a t -> (int -> 'a -> bool) -> bool

(** true if every element in self returns true. stops proccessing at first 'false'.
returns true if no elements in the iterator. *)
val forAll: 'a t -> ('a -> bool) -> bool

val forAll2: 'a t -> 'b t -> ('a -> 'b -> bool) -> bool

val forAllIdx: 'a t -> (int -> 'a -> bool) -> bool

(** find the first element that satisfies the predicate or None if not found *)
val find: 'a t -> ('a -> bool) -> 'a option

val findIdx: 'a t -> (int -> 'a -> bool) -> (int * 'a) option

(** flatten an iterator of iterators. returns all the elements of the first
elements, followed by the second, etc. runs out of elements when the last
iterator is spent and there are no more iterators. *)
val flatten: 'a t t -> 'a t

(** count the number of times the predicate returns true *)
val countInstances: 'a t -> ('a -> bool) -> int

(** new iterator returns all elements of iter1, then all elements in iter2 *)
val append: 'a t -> 'a t -> 'a t

(** compares two iterators. returns the first non-zero value returned by
the compare function applied to the next() elements of the iterators. 
If the iterators are different lenghts, the one that still has elements will
be called greater.*)
val compare: 'a t -> 'a t -> ('a -> 'a -> int) -> int

(** does a filtering where only elems that haven't been returned before will
pass through. *)
val uniques: 'a t -> 'a Comparer.t -> 'a t

(** same as fromList(toList iter) *)
val renderCopy: 'a t -> 'a t

