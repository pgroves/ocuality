(** Ruby style Blocks *)

(** {5 HOF Translation} *)

(** creates an internal index iterator (that starts at zero) and feeds its
value to the application function each time, and then increments it. Therefore
to make a List.iter method that takes in the position of the element in 
the list could be accomplished as:

	let idxAppFun:(int -> 'a -> unit) = fun index elem -> () in
	let appFun:('a -> unit) = Block.addIndexClosure idxAppFun in
	List.iter appFun myList

*)
val addIndexClosure: (int -> 'a -> 'b) -> ('a -> 'b)

(** {5 Multiple Calls} *)

(** call the input function n times *)
val timesDo: int -> (unit -> unit) -> unit

(** loop for while(predicate) do (body) *)
val whileDo: (unit -> bool) -> (unit -> unit) -> unit

(** loop for do(body) while(predicate) *)
val doWhile: (unit -> bool) -> (unit -> unit) -> unit

(** loop for: for(idx = 0; idx < maxIdx; idx++) do(body) *)
val forDo: int -> (int -> unit) -> unit
