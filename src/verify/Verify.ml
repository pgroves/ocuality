
let success ~label = begin
	CheckHandler.declareSuccess ~label:label
	end

let failure ~label = begin
	CheckHandler.declareFailure ~label:label
	end

let crash ~label = begin
	failure label;
	(** if the failure did not cause an exception, we must force one *)
	failwith "Last Failure was Unrecoverable. Program Terminated."
	end

let crashOnException ~label ~block = begin
	let result = begin
		try 
			block ()
		with _ -> crash ~label:label
		end
	in
	result
	end

let predicate ~label ~pred = begin
	if(pred) then
		success ~label:label
	else
		failure ~label:label
	end

let doesFail ~label ~block = begin
	CheckHandler.startNoFail ();
	begin
		try
			block ();
		with _ -> Log.info "Verify.doesFail block exited with exception"
	end;
	match (CheckHandler.stopNoFail ()) with
	|false -> begin
		failure ("Verify.doesFail: expected failure, had none. " ^ label)
		end
	|true -> begin
		success ("Verify.doesFail: failed as expected. " ^ label)
		end
	end

	
(************ General Numeric *)
let gt ~label ~cmp ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 > x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.greaterThan cmp x1 x2)
	end

let gte ~label ~cmp  ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 >= x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.greaterThanOrEqual cmp x1 x2)
	end
	
let lt ~label  ~cmp ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 < x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.lessThan cmp x1 x2)
	end
	
let lte ~label ~cmp ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 <= x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.lessThanOrEqual cmp x1 x2)
	end
	
let areEqual ~label  ~cmp ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 == x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.areEqual cmp x1 x2)
	end

let notEqual ~label  ~cmp ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (x1 != x2) for values: (x1 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x1);
	Buffer.add_string sBuf "), (x2 = ";
	Buffer.add_string sBuf (Comparer.toString cmp x2);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.notEqual cmp x1 x2)
	end


let inRange ~label ~cmp ~x ~min ~max = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: (min <= x < max) for values: (x = ";
	Buffer.add_string sBuf (Comparer.toString cmp x);
	Buffer.add_string sBuf "), (min = ";
	Buffer.add_string sBuf (Comparer.toString cmp min);
	Buffer.add_string sBuf "), (max = ";
	Buffer.add_string sBuf (Comparer.toString cmp max);
	Buffer.add_string sBuf ")";
	let innerLabel = Buffer.contents sBuf in
	predicate innerLabel (Comparer.inRange cmp x min max)
	end

(** pretty prints a list to a string buffer. uses hash codes as the values *)
let printListToBuffer buffer printFunOpt lst = begin
	let valToString = begin
		match printFunOpt with
		|None -> fun x -> string_of_int (Hashtbl.hash x)
		|Some(fn) -> fn
		end
	in
	let printVal x =  Buffer.add_string buffer (valToString x) in
	let rec printList ll = begin
		match ll with
		|[] -> ()
		|[x] -> printVal x
		|hd::tl -> printVal hd; Buffer.add_string buffer "; "; printList tl
		end
	in
	Buffer.add_char buffer '[';
	printList lst;
	Buffer.add_char buffer ']';
	end

let listLength ~label ~intVerifyFun ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: List Length, (x1 = expected), (x2 = observed) :: ";
	let innerLabel = Buffer.contents sBuf in
	intVerifyFun ~label:innerLabel ~x1:x1 ~x2:(List.length x2)
	end

let listEndsWith ~label ?printFun ~x ~endOfX = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: List x = ";
	printListToBuffer sBuf printFun x;
	Buffer.add_string sBuf " should end with the values of endOfX = ";
	printListToBuffer sBuf printFun endOfX;
	let innerLabel = Buffer.contents sBuf in
	(* loop tells if lst1 STARTS with lst2 *)
	let rec loop lst1 lst2 = begin
		match lst1, lst2 with
		|(_, []) -> true
		|([], _) -> false
		|(hd1::tl1, hd2::tl2) -> if(hd1 = hd2) then loop tl1 tl2 else false
		end
	in
	let pred = loop (List.rev x) (List.rev endOfX) in
	predicate innerLabel pred
	end


let listEquals ~label ?printFun ~x1 ~x2 = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf label;
	Buffer.add_string sBuf " :: List x1 = ";
	printListToBuffer sBuf printFun x1;
	Buffer.add_string sBuf "  x2 = ";
	printListToBuffer sBuf printFun x2;
	let innerLabel = Buffer.contents sBuf in
	(* loop tells if lst1 STARTS with lst2 *)
	let rec loop lst1 lst2 = begin
		match lst1, lst2 with
		|[], [] -> true
		|(_, []) -> false
		|([], _) -> false
		|(hd1::tl1, hd2::tl2) -> if(hd1 = hd2) then loop tl1 tl2 else false
		end
	in
	let pred = loop x1 x2 in
	predicate innerLabel pred
	end


let notImplemented ~moduleName ~methodName = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf "Method Not Implemented:";
	Buffer.add_string sBuf moduleName;
	Buffer.add_char sBuf '.';
	Buffer.add_string sBuf methodName;
	crash ~label:(Buffer.contents sBuf)
	end

let notVerified ~moduleName ~methodName = begin
	let sBuf = Buffer.create 80 in
	Buffer.add_string sBuf "Verifier Not Implemented:";
	Buffer.add_string sBuf moduleName;
	Buffer.add_char sBuf '.';
	Buffer.add_string sBuf methodName;
	failure ~label:(Buffer.contents sBuf)
	end
	
