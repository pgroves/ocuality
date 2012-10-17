
type 'a t = {
	hasNextFun: (unit -> bool);
	nextFun: (unit -> 'a);
	}

let create ~hasNextFun ~nextFun = begin
	let newSelf = {
		hasNextFun = hasNextFun;
		nextFun = nextFun;
		}
	in
	newSelf
	end

(*let createFixedSize ~size ~nextFun = begin
	let countDown = ref size in
	let hasNextFn = fun () -> !countDown > 0 in
	let nextFn = fun () -> begin
		countDown := !countDown - 1; 
		let elem = nextFun () in
		elem
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end
*)
let createIndexed ~size ~nextFun = begin
	let countDown = ref size in
	let hasNextFn = fun () -> !countDown > 0 in
	let nextFn = fun () -> begin
		let elem = nextFun (size - !countDown) in
		countDown := !countDown - 1; 
		elem
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let createFixedSize ~size ~nextFun = begin
	createIndexed ~size:size ~nextFun:(fun _ -> nextFun ())
	end


let hasNext self = self.hasNextFun ()

let next self = begin
	match hasNext self with
	|true -> self.nextFun ()
	|false -> Verify.crash ~label:"Iterator.next: no more elems"
	end


let fromList lst = begin
	let remainderLst = ref lst in
	let hasNextFn = fun () -> begin
		match !remainderLst with
		|[] -> false
		|_ -> true
		end
	in
	let nextFn = fun () -> begin
		match !remainderLst with
		|[] -> Verify.crash ~label:"Iterator.fromList.next: no more elems"
		|hd::tl -> remainderLst := tl; hd
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let fromArray ary = begin
	let arySize = Array.length ary in
	let idx = ref 0 in
	let hasNextFn = fun () -> (!idx < arySize) in
	let nextFn = fun () -> begin
		let elem = Array.get ary !idx in
		idx := !idx + 1;
		elem
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let fromHashtbl hashtbl = begin
	let collectedLst = ref [] in
	let visitPair = fun key value -> collectedLst := (key, value)::!collectedLst
	in
	Hashtbl.iter visitPair hashtbl;
	let newSelf = fromList !collectedLst in
	newSelf
	end

let fromQueue queue = begin
	let hasNextFn = fun () -> not (Queue.is_empty queue) in
	let nextFn = fun () -> Queue.pop queue in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let toList self = begin
	let rec loop iter accLst = begin
		match (hasNext iter) with
		|true -> loop iter ((next iter)::accLst)
		|false -> accLst
		end
	in
	let revLst = loop self [] in
	let lst = List.rev revLst in
	lst
	end

let map self mapFun = begin
	let nextFn = fun () -> (mapFun (next self)) in
	let hasNextFn = fun () -> (hasNext self) in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let map2 self1 self2 mapFun = begin
	let nextFn = fun () -> (mapFun (next self1) (next self2)) in
	let hasNextFn = fun () -> (hasNext self1) in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let mapIdx self mapIdxFun = begin
	let mapFn = Block.addIndexClosure mapIdxFun in
	map self mapFn
	end

let expand self expandFun = begin
	let outElems = ref [] in
	let hasNextFn () = begin	
		match !outElems with
		|[] -> hasNext self
		|lst -> true
		end
	in
	let rec nextFn () = begin
		match !outElems with
		|[] ->  begin
			match hasNext self with
			|false -> Verify.crash ~label:"Iter.expand: sourceIter empty"
			|true -> outElems := (expandFun (next self)); nextFn ()
			end
		|hd::tl -> outElems := tl; hd
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let compress self compressFun = begin
	let nextOutElem = ref None in
	(** pops off elements of the sourceIter and feeds them to the compress
	function until it gives back a 'Some' value, and sets nextOutElem to that.
	If it reaches the end of the sourceIter, sets nextOutElem to None *)
	let rec advanceNextOutElem = fun () -> begin
		match hasNext self with
		|false -> nextOutElem := None
		|true -> begin
			let nextSourceElem = next self in
			match (compressFun nextSourceElem) with
			|None -> advanceNextOutElem ()
			|Some(elem) -> nextOutElem := Some(elem)
			end
		end
	in	
	let hasNextFn () = begin
		match !nextOutElem with
		|None -> false
		|Some(_) -> true
		end
	in
	let nextFn () = begin
		match !nextOutElem with
		|None -> Verify.crash ~label:"Iterator.compress.nextFn: no next elem"
		|Some(elem) -> advanceNextOutElem (); elem
		end
	in
	advanceNextOutElem ();
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	end

let filter self acceptFun = begin
	let compressFn = fun elem -> begin
		match acceptFun elem with
		|false -> None
		|true -> Some(elem)
		end
	in
	let newSelf = compress self compressFn in
	newSelf
	end

let forEach self visitFun = begin
	while(hasNext self) do 
		visitFun (next self) 
		done;
	()
	end

let forEachIdx self visitIdxFun = begin
	let visitFun = Block.addIndexClosure visitIdxFun in
	forEach self visitFun;
	()
	end

let forEach2 iter1 iter2 visitFun = begin
	(**iter1 is considered the master. iter2 will be popped inside an
	application function*)
	let visitFun1 = fun elem1 -> begin
		let elem2 = next iter2 in
		visitFun elem1 elem2;
		()
		end
	in
	forEach iter1 visitFun1;
	Verify.predicate
		~label:"iter2 has no more elements"
		~pred:(not (hasNext iter2));
	()
	end

let forEachIdx2 iter1 iter2 visitIdxFun = begin
	let visitFun = Block.addIndexClosure visitIdxFun in
	forEach2 iter1 iter2 visitFun
	end

let rec exists self acceptFun = begin
	match hasNext self with
	|false -> false
	|true -> begin
		match acceptFun (next self) with
		|true -> true
		|false -> exists self acceptFun
		end
	end

let exists2 iter1 iter2 acceptFun = begin
	(**iter1 is considered the master. iter2 will be popped inside an
	acceptance function*)
	let accept2Fun = fun elem1 -> begin
		let elem2 = next iter2 in
		let accept = acceptFun elem1 elem2 in
		accept
		end
	in
	let doesExist = exists iter1 accept2Fun in
	Verify.predicate
		~label:"exists2:iter2 has no more elements"
		~pred:(doesExist || (not (hasNext iter2)));
	doesExist
	end

let existsIdx self acceptIdxFun = begin
	let acceptFun = Block.addIndexClosure acceptIdxFun in
	let doesExist = exists self acceptFun in
	doesExist
	end

let forAll self acceptFun = begin
	(* we just need to find one that doesn't satisfy predFn to declare failure, 
	otherwise it's true. so this is the inverse of the 'exists' fn. *)
	let invAcceptFun = fun elem -> not (acceptFun elem) in
	let trueForAll = not (exists self invAcceptFun) in
	trueForAll
	end


let forAll2 iter1 iter2 accept2Fn = begin
	let invAcceptFun = fun elem1 elem2 -> not (accept2Fn elem1 elem2) in
	let trueForAll = not (exists2 iter1 iter2 invAcceptFun) in
	(*Verify.predicate
		~label:"forAll2: iter2 has no more elements"
		~pred:(trueForAll || (not (hasNext iter2)));
	*)
	trueForAll
	end


let forAllIdx self acceptIdxFun = begin
	let acceptFun = Block.addIndexClosure acceptIdxFun in
	let trueForAll = forAll self acceptFun in
	trueForAll
	end

let rec find self acceptFun = begin
	match (hasNext self) with
	|false -> None (** end of iter, didn't find it *)
	|true -> begin
		let elem = next self in
		match (acceptFun elem) with
		|true -> Some(elem) (** only need to find one and we can break *)
		|false -> find self acceptFun (** try the next one *)
		end
	end

let findIdx self acceptFun = begin
	let acceptIdxPairFn = fun (idx, elem) -> acceptFun idx elem in
	let idxPairIter = mapIdx self (fun idx elem -> (idx, elem)) in
	find idxPairIter acceptIdxPairFn
	end

let flatten iterIter = begin
	let expandFn = fun innerIter -> toList innerIter in
	let flatIter = expand iterIter expandFn in
	flatIter
	end

let append iter1 iter2 = begin
	let hasNextFn  = fun () -> ((hasNext iter1) || (hasNext iter2)) in
	let nextFn = fun () -> begin
		match hasNext iter1 with
		|true -> next iter1
		|false -> next iter2
		end
	in
	let newSelf = create ~hasNextFun:hasNextFn ~nextFun:nextFn in
	newSelf
	
	end

let countInstances self acceptFun = begin
	let tally = ref 0 in
	let visitElem = fun elem -> begin
		if(acceptFun elem) then (tally := !tally + 1) else ();
		end
	in
	forEach self visitElem;
	!tally
	end


let rec compare iter1 iter2 cmpFn = begin
	match (hasNext iter1), (hasNext iter2) with
	|false, false -> 0 (* equal *)
	|true, false -> 1 (* iter1Greater *)
	|false, true -> (-1) (* iter2 Greater *)
	|true, true -> begin
		let elem1 = next iter1 in
		let elem2 = next iter2 in
		let cmpResult = cmpFn elem1 elem2 in
		match (cmpResult = 0) with
		|false-> cmpResult
		|true -> compare iter1 iter2 cmpFn
		end
	end


let uniques self cmp = begin
	let prevUniquesStack = ref [] in
	(** this returns true if the elem has not been returned before. *)
	let filterCheck = fun elem -> begin
		(** this looks through the remaining uniquesStack to see if our
		candidate elem has already been returned. *)
		let rec loopLookup = fun elem uniquesStack -> begin
			match uniquesStack with
			|[] -> begin (** not found in previously seen uniques, it's a new uniq*)
				prevUniquesStack := elem::!prevUniquesStack; 
				true
				end
			|x::xs -> begin
				match Comparer.areEqual cmp x elem with
				|true -> false (** if they're equal, do not let it through filter*)
				|false -> loopLookup elem xs
				end
			end
		in
		loopLookup elem !prevUniquesStack
		end
	in
	let newSelf = filter self filterCheck in
	newSelf
	end

let renderCopy self = fromList (toList self)



