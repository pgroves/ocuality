(**
	Unit tests for Block 

*)



(**  *)
let test1 () = TestCase.createAtomic "addIndexClosure" (fun () -> begin
	let mapIdxFn = fun idx x -> idx * x in
	let mapFn = Block.addIndexClosure mapIdxFn in

	let lst = [1; 1; 1; 1] in
	let obsMappedLst = List.map mapFn lst in
	let expMappedLst = [0; 1; 2; 3] in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs mapped lst"
		~cmp:cmp
		~x1:expMappedLst
		~x2:obsMappedLst;
	()
	end)
(**  *)
let test2 () = TestCase.createAtomic "timesDo" (fun () -> begin
	let tally = ref 0 in
	let loop = fun () -> tally := !tally + 1 in
	Block.timesDo 0 loop;
	Verify.areEqual
		~label:"exp, obs counter after 0 revs"
		~cmp:Comparer.ints
		~x1:0
		~x2:!tally;

	tally := 0;
	Block.timesDo 5 loop;
	Verify.areEqual
		~label:"exp, obs counter after 5 revs"
		~cmp:Comparer.ints
		~x1:5
		~x2:!tally;
	end)

(**  *)
let test3 () = TestCase.createAtomic "whileDo" (fun () -> begin
	let count = ref 0 in
	let pred = fun () -> !count < 4 in
	let body = fun () -> count := !count + 1 in
	Block.whileDo pred body;
	Verify.areEqual
		~label:"exp, obs count"
		~cmp:Comparer.ints
		~x1:4
		~x2:!count;
	()
	end)

(**  *)
let test4 () = TestCase.createAtomic "doWhile" (fun () -> begin
	let count = ref 0 in
	let pred = fun () -> ((!count < 4) && (!count > 0)) in
	let body = fun () -> count := !count + 1 in
	Block.doWhile pred body;
	Verify.areEqual
		~label:"exp, obs count"
		~cmp:Comparer.ints
		~x1:4
		~x2:!count;
	()
	end)

(**  *)
let test5 () = TestCase.createAtomic "forDo" (fun () -> begin
	let tally = ref 0 in
	let max = 5 in
	let body = fun idx -> tally := !tally + idx in
	Block.forDo max body;
	Verify.areEqual
		~label:"exp, obs tally"
		~cmp:Comparer.ints
		~x1:10
		~x2:!tally;
	()
	end)

(*
(**  *)
let test6 ()  = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test7 () = TestCase.createAtomic "" (fun () -> begin
	end)
*)

let testSuite () = begin
	let tests = [
		test1 (); 
		test2 (); 
		test3 (); 
		test4 (); 
		test5 (); 
		(*
		test6 (); 
		test7 ();
		*)
		]
	in
	let suite = TestCase.createSuite "Block" tests in
	suite
	end

