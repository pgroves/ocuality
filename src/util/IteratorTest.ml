(**
	Unit tests for Iterator 

*)



(**  *)
let test1 () = TestCase.createAtomic "tofromlist" (fun () -> begin
	let cmp = ListUtil.makeComparer Comparer.ints in
	let origLst = [1; 2; 3; 4] in
	let roundtripLst = Iterator.toList (Iterator.fromList origLst) in
	Verify.areEqual
		~label:"exp, obs roundtrip list"
		~cmp:cmp
		~x1:origLst
		~x2:roundtripLst;
	()
	end)
(**  *)
let test2 () = TestCase.createAtomic "array" (fun () -> begin
	let origLst = [1; 2; 3; 4] in
	let origAry = Array.of_list origLst in
	let iter = Iterator.fromArray origAry in
	let roundtripLst = Iterator.toList iter in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs roundtrip list"
		~cmp:cmp
		~x1:origLst
		~x2:roundtripLst;
	()
	end)

(**  *)
let test3 () = TestCase.createAtomic "hashtbl" (fun () -> begin
	let tbl = Hashtbl.create 25 in
	Hashtbl.add tbl 1 "a";
	Hashtbl.add tbl 2 "b";
	Hashtbl.add tbl 3 "c";

	let iter = Iterator.fromHashtbl tbl in
	let rawLst = Iterator.toList iter in

	let compareFun = fun (k1, v1) (k2, v2) -> compare k1 k2 in
	let toStringFun = fun (k1, v1) -> (string_of_int k1) ^ "," ^ v1 in
	let cmp = Comparer.create ~toStringFun:toStringFun ~compareFun:compareFun in

	let sortedLst = List.sort compareFun rawLst in
	let expSortedLst = [(1,"a"); (2, "b"); (3, "c")] in

	let listCmp = ListUtil.makeComparer cmp in

	Verify.areEqual
		~label:"exp, obs sorted roundtrip list"
		~cmp:listCmp
		~x1:expSortedLst
		~x2:sortedLst;
	()
	end)

(**  *)
let test4 () = TestCase.createAtomic "map" (fun () -> begin
	let origLst = [1; 2; 3; 4] in
	let mapFn = fun x -> 2 * x in
	let expLst = [2; 4; 6; 8] in
	let obsIter = Iterator.map (Iterator.fromList origLst) mapFn in
	let obsLst = Iterator.toList obsIter in
	let lstCmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs transformed list"
		~cmp: lstCmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

(**  *)
let test5 () = TestCase.createAtomic "expand" (fun () -> begin
	let origLst = [1; 2; 3] in
	let expandFun = fun x -> Array.to_list (Array.make x x) in
	let expLst = [1; 2; 2; 3; 3; 3] in

	let obsIter = Iterator.expand (Iterator.fromList origLst) expandFun in
	let obsLst = Iterator.toList obsIter in
	let lstCmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs expanded list"
		~cmp:lstCmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

(**  *)
let test6 ()  = TestCase.createAtomic "compress" (fun () -> begin
	let tally = ref 0 in
	let compressFn = fun x -> begin
		tally := x + !tally;
		match !tally > 5 with
		|true -> begin
			let retVal = !tally in
			tally := 0;
			Some(retVal)
			end
		|false -> None
		end
	in
	Log.test "First case: last returned compressOutput is a None";
	let origIter1 = Iterator.fromList [2; 3; 4; 2; 2; 3; 7; 3] in
	let expLst1 = [9; 7; 7] in
	let obsIter1 = Iterator.compress origIter1 compressFn in
	let obsLst1 = Iterator.toList obsIter1 in
	let lstCmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs compressed list, case 1"
		~cmp:lstCmp
		~x1:expLst1
		~x2:obsLst1;

	Log.test "Second case: last returned compressOutput is a Some";
	tally := 0;
	let origIter2 = Iterator.fromList [2; 3; 4; 2; 2; 3; 7; 8] in
	let expLst2 = [9; 7; 7; 8] in
	let obsIter2 = Iterator.compress origIter2 compressFn in
	let obsLst2 = Iterator.toList obsIter2 in
	Verify.areEqual
		~label:"exp, obs compressed list, case 2"
		~cmp:lstCmp
		~x1:expLst2
		~x2:obsLst2;



	end)

(**  *)
let test7 () = TestCase.createAtomic "filter" (fun () -> begin
	let origIter = Iterator.fromList [0; 3; 2; 5] in
	let filterFn = fun x -> x < 3 in
	let obsIter = Iterator.filter origIter filterFn in
	let obsLst = Iterator.toList obsIter in
	let expLst = [0; 2] in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp obs, filtered list (x < 3)"
		~cmp:cmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

(**  *)
let test8 () = TestCase.createAtomic "forEach" (fun () -> begin
	let origIter = Iterator.fromList [1; 2; 3; 4] in
	let tally = ref 0 in
	let visitFn = fun x -> tally := !tally + x in
	Iterator.forEach origIter visitFn;
	let expTally = 10 in
	let obsTally = !tally in
	Verify.areEqual
		~label:"exp, obs tally"
		~cmp: Comparer.ints
		~x1:expTally
		~x2:obsTally;
	()
	end)

(**  *)
let test9 () = TestCase.createAtomic "forEachIdx" (fun () -> begin
	let origIter = Iterator.fromList [1; 2; 3; 4] in
	let tally = ref 0 in
	let visitFn = fun idx x -> tally := !tally + x + idx in
	Iterator.forEachIdx origIter visitFn;
	let expTally = 16 in
	let obsTally = !tally in
	Verify.areEqual
		~label:"exp, obs tally"
		~cmp: Comparer.ints
		~x1:expTally
		~x2:obsTally;
	()
	end)


(**  *)
let test10 () = TestCase.createAtomic "forEach2" (fun () -> begin
	let origIter1 = Iterator.fromList [1; 2; 3; 4] in
	let origIter2 = Iterator.fromList [1; 1; 1; 0] in
	let tally = ref 0 in
	let visitFun = fun x1 x2 -> tally := !tally + x1 * x2 in
	Iterator.forEach2 origIter1 origIter2 visitFun;
	let obsTally = !tally in
	let expTally = 6 in
	Verify.areEqual
		~label:"exp, obs tally"
		~cmp:Comparer.ints
		~x1:expTally
		~x2:obsTally;

	Log.test "now check that forEach2 fails if either iterator is longer ";
	let origIter1b = Iterator.fromList [1; 2; 3] in
	let origIter2b = Iterator.fromList [1; 1; 1] in

	Verify.doesFail
		~label:"iter1 shorter than iter2"
		~block:(fun () -> Iterator.forEach2 origIter1b origIter2 visitFun; ());

	Verify.doesFail
		~label:"iter2 shorter than iter1"
		~block:(fun () -> Iterator.forEach2 origIter1 origIter2b visitFun; ());
	()
	end)


(**  *)
let test11 () = TestCase.createAtomic "forEachIdx2" (fun () -> begin
	let origIter1 = Iterator.fromList [1; 2; 3; 4] in
	let origIter2 = Iterator.fromList [1; 1; 1; 0] in
	let tally = ref 0 in
	let visitFun = fun idx x1 x2 -> tally := !tally + x1 * x2 * idx in
	Iterator.forEachIdx2 origIter1 origIter2 visitFun;
	let obsTally = !tally in
	let expTally = 8 in
	Verify.areEqual
		~label:"exp, obs tally"
		~cmp:Comparer.ints
		~x1:expTally
		~x2:obsTally;

	Log.test "now check that forEach2 fails if either iterator is longer ";
	let origIter1b = Iterator.fromList [1; 2; 3] in
	let origIter2b = Iterator.fromList [1; 1; 1] in

	Verify.doesFail
		~label:"iter1 shorter than iter2"
		~block:(fun () -> Iterator.forEachIdx2 origIter1b origIter2 visitFun; ());

	Verify.doesFail
		~label:"iter2 shorter than iter1"
		~block:(fun () -> Iterator.forEachIdx2 origIter1 origIter2b visitFun; ());
	()
	end)


(**  *)
let test12 () = TestCase.createAtomic "exists" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [0; 1; 2; 3] in
	let iter3 = Iterator.fromList [0; 1; 3; 2] in
	let iter4 = Iterator.fromList [0; 3; 1] in
	let acceptFun = fun x -> x = 2 in
	Verify.predicate
		~label:"empty iterator has no 2?"
		~pred:(not (Iterator.exists iter1 acceptFun));
	Verify.predicate
		~label:"iterator with 2 in middle a has 2?"
		~pred:(Iterator.exists iter2 acceptFun);
	Verify.predicate
		~label:"iterator with 2 at end has a 2?"
		~pred:(Iterator.exists iter3 acceptFun);
	Verify.predicate
		~label:"iterator with no 2 has no 2?"
		~pred:(not (Iterator.exists iter4 acceptFun));
	()
	end)


(**  *)
let test13 () = TestCase.createAtomic "exists2" (fun () -> begin
	let iter1 = Iterator.fromList [1; 2; 3; 4] in
	let iter2 = Iterator.fromList [1; 1; 1; 0] in
	let acceptFun = fun x1 x2 -> ((x1 - x2) = 2) in
	let acceptFun2 = fun x1 x2 -> ((x1 - x2) = 5) in
	Verify.predicate
		~label:"exists2 iter1:iter2"
		~pred:(Iterator.exists2 iter1 iter2 acceptFun);
	let iter7 = Iterator.fromList [1; 2; 3; 4] in
	let iter8 = Iterator.fromList [1; 1; 1; 0] in
	Verify.predicate
		~label:"not exists2 iter7:iter8, acceptFun2"
		~pred:(not (Iterator.exists2 iter7 iter8 acceptFun2));


	Log.test "now check that exists2 fails if either iterator is longer ";

	let iter3 = Iterator.fromList [1; 2; 3] in
	let iter4 = Iterator.fromList [1; 1; 0; 0] in
	Verify.doesFail
		~label:"iter1 shorter than iter2"
		~block:(fun () -> ignore (Iterator.exists2 iter3 iter4 acceptFun); ());

	let iter5 = Iterator.fromList [1; 1; 0; 0] in
	let iter6 = Iterator.fromList [1; 2; 3] in
	Verify.doesFail
		~label:"iter2 shorter than iter1"
		~block:(fun () -> ignore(Iterator.exists2 iter5 iter6 acceptFun); ());
	()

	end)


(**  *)
let test14 () = TestCase.createAtomic "existsIdx" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [0; 1; 2; 3] in
	let iter3 = Iterator.fromList [0; 1; 3; 1] in
	let iter4 = Iterator.fromList [0; 2; 1] in
	let acceptFun = fun idx x -> x + idx  = 4 in
	Verify.predicate
		~label:"empty iterator has no 2?"
		~pred:(not (Iterator.existsIdx iter1 acceptFun));
	Verify.predicate
		~label:"iterator with 2 in middle a has 2?"
		~pred:(Iterator.existsIdx iter2 acceptFun);
	Verify.predicate
		~label:"iterator with 2 at end has a 2?"
		~pred:(Iterator.existsIdx iter3 acceptFun);
	Verify.predicate
		~label:"iterator with no 2 has no 2?"
		~pred:(not (Iterator.existsIdx iter4 acceptFun));
	()
	end)


(**  *)
let test15 () = TestCase.createAtomic "forAll" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [1; 3; 4; 5] in
	let iter3 = Iterator.fromList [1; 1; 3; 2] in
	let iter4 = Iterator.fromList [1; 3; 5] in
	let acceptFun = fun x -> x mod 2 = 1 in
	Verify.predicate
		~label:"empty iterator has all ods?"
		~pred:(Iterator.forAll iter1 acceptFun);
	Verify.predicate
		~label:"iterator with even in middle a has a not odd?"
		~pred:(not (Iterator.forAll iter2 acceptFun));
	Verify.predicate
		~label:"iterator with even at end has an odd?"
		~pred:(not(Iterator.forAll iter3 acceptFun));
	Verify.predicate
		~label:"iterator with all odd all odd?"
		~pred:(Iterator.forAll iter4 acceptFun);
	()
	end)


(**  *)
let test16 () = TestCase.createAtomic "forAll2" (fun () -> begin
	let iter1 = Iterator.fromList [2; 3; 4; 5] in
	let iter2 = Iterator.fromList [0; 1; 2; 3] in
	let acceptFun1 = fun x1 x2 -> ((x1 - x2) = 2) in
	let acceptFun2 = fun x1 x2 -> ((x1 + x2) = 2) in
	Verify.predicate
		~label:"forAll2 iter1:iter2, acceptFun1"
		~pred:(Iterator.forAll2 iter1 iter2 acceptFun1);
	let iter7 = Iterator.fromList [2; 3; 4; 5] in
	let iter8 = Iterator.fromList [0; 1; 2; 3] in
	Verify.predicate
		~label:"not forAll2 iter7:iter8, acceptFun2"
		~pred:(not (Iterator.forAll2 iter7 iter8 acceptFun2));

	Log.test "now check that forAll2 fails if either iterator is longer ";

	let iter3 = Iterator.fromList [2; 3; 4] in
	let iter4 = Iterator.fromList [0; 1; 2; 0] in
	Verify.doesFail
		~label:"iter1 shorter than iter2"
		~block:(fun () -> ignore (Iterator.forAll2 iter3 iter4 acceptFun1); ());

	let iter5 = Iterator.fromList [3; 4; 5; 0] in
	let iter6 = Iterator.fromList [1; 2; 3] in
	Verify.doesFail
		~label:"iter2 shorter than iter1"
		~block:(fun () -> ignore(Iterator.forAll2 iter5 iter6 acceptFun1); ());
	()
	end)


(**  *)
let test17 () = TestCase.createAtomic "forAllIdx" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [1; 2; 3; 4] in
	let iter3 = Iterator.fromList [1; 2; 3; 3] in
	let iter4 = Iterator.fromList [1; 1; 3; 4] in
	let acceptFun = fun idx x -> ((idx + x) mod 2) = 1 in
	Verify.predicate
		~label:"empty iterator all odd?"
		~pred:(Iterator.forAllIdx iter1 acceptFun);
	Verify.predicate
		~label:"iterator with no odds is all odds?"
		~pred:(Iterator.forAllIdx iter2 acceptFun);
	Verify.predicate
		~label:"iterator with even at end not all odd?"
		~pred:(not(Iterator.forAllIdx iter3 acceptFun));
	Verify.predicate
		~label:"iterator with even in middle not all odd?"
		~pred:(not (Iterator.forAllIdx iter4 acceptFun));
	()
	end)


(**  *)
let test18 () = TestCase.createAtomic "find" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [1; 2; 3; 4] in
	let iter3 = Iterator.fromList [1; 2; 3; 3] in
	let acceptFun = fun x -> x = 4 in
	let cmp = Comparer.makeOption Comparer.ints in

	let expFind1 = None in
	let obsFind1 = Iterator.find iter1 acceptFun in
	Verify.areEqual
		~label:"exp, obs found value, iter1"
		~cmp:cmp
		~x1:expFind1
		~x2:obsFind1;

	let expFind2 = Some(4) in
	let obsFind2 = Iterator.find iter2 acceptFun in
	Verify.areEqual
		~label:"exp, obs found value, iter2"
		~cmp:cmp
		~x1:expFind2
		~x2:obsFind2;

	let expFind3 = None in
	let obsFind3 = Iterator.find iter3 acceptFun in
	Verify.areEqual
		~label:"exp, obs found value, iter3"
		~cmp:cmp
		~x1:expFind3
		~x2:obsFind3;

	end)


(**  *)
let test19 () = TestCase.createAtomic "countInstances" (fun () -> begin
	let iter1 = Iterator.fromList [] in
	let iter2 = Iterator.fromList [1; 2; 3; 4] in
	let iter3 = Iterator.fromList [1; 2; 4; 4] in
	let acceptFun = fun x -> x = 4 in

	Verify.areEqual
		~label:"exp, obs: num 4's in iter1"
		~cmp:Comparer.ints
		~x1:0
		~x2:(Iterator.countInstances iter1 acceptFun);

	Verify.areEqual
		~label:"exp, obs num 4's in iter2"
		~cmp:Comparer.ints
		~x1:1
		~x2:(Iterator.countInstances iter2 acceptFun);

	Verify.areEqual
		~label:"exp, obs num 4's in iter3"
		~cmp:Comparer.ints
		~x1:2
		~x2:(Iterator.countInstances iter3 acceptFun);



	end)


(**  *)
let test20 () = TestCase.createAtomic "compare" (fun () -> begin
	(** compares int iterators *)
	let iterCmp = Comparer.create
		~toStringFun: (fun iter -> ignore iter; "Iterator.toString can't impl")
		~compareFun:(fun i1 i2 -> Iterator.compare i1 i2 compare)
	in

	let iter1 = Iterator.fromList [1; 2; 3] in
	let iter2 = Iterator.fromList [1; 2; 4] in
	Verify.lt
		~label:"iter1, iter2"
		~cmp:iterCmp
		~x1:iter1
		~x2:iter2;

	let iter3 = Iterator.fromList [1; 2; 4] in
	let iter4 = Iterator.fromList [1; 2; 3] in
	Verify.gt
		~label:"iter3, iter4"
		~cmp:iterCmp
		~x1:iter3
		~x2:iter4;

	let iter5 = Iterator.fromList [1; 2; 3; 0] in
	let iter6 = Iterator.fromList [1; 2; 3] in
	Verify.gt
		~label:"iter5, iter6"
		~cmp:iterCmp
		~x1:iter5
		~x2:iter6;

	let iter7 = Iterator.fromList [1; 2; 3] in
	let iter8 = Iterator.fromList [1; 2; 3; 0] in
	Verify.lt
		~label:"iter7, iter8"
		~cmp:iterCmp
		~x1:iter7
		~x2:iter8;
	()
	end)


(**  *)
let test21 () = TestCase.createAtomic "renderCopy" (fun () -> begin
	let lst = [1; 2; 3] in
	let origIter = Iterator.fromList lst in
	let iterCpy = Iterator.renderCopy origIter in
	let obsLstCopy = Iterator.toList iterCpy in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs lst copy"
		~cmp:cmp
		~x1:lst
		~x2:obsLstCopy;
	()
	end)


(**  *)
let test22 () = TestCase.createAtomic "createFixedSize" (fun () -> begin
	let lst = [1; 2; 3] in
	let hdTracker = ref lst in
	let nextFn = fun () -> begin
		match !hdTracker with
		|[] -> Verify.crash ~label:"createFixedSize: no more elems"
		|hd::tl -> hdTracker := tl; hd
		end
	in
	let iter = Iterator.createFixedSize ~size:3 ~nextFun:nextFn in
	let collectorTracker = ref [] in
	let blockLoop = fun () -> begin
		ignore (Iterator.hasNext iter);
		collectorTracker := (Iterator.next iter)::!collectorTracker;
		ignore (Iterator.hasNext iter);
		end
	in
	let pred = fun () -> Iterator.hasNext iter in
	Block.whileDo pred blockLoop;
	
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs collected list"
		~cmp:cmp
		~x1:(List.rev lst)
		~x2:(!collectorTracker);
	()
	end)


let test23 () = TestCase.createAtomic "uniques" (fun () -> begin

	let checkUniques origSeqLst expUniqueLst = begin
		let origSeq = Iterator.fromList origSeqLst in
		let obsUniqueIter = Iterator.uniques origSeq Comparer.ints in
		let obsUniqueLst = Iterator.toList obsUniqueIter in

		let cmp = ListUtil.makeComparer Comparer.ints in
		Verify.areEqual
			~label:"exp, obs uniques"
			~cmp:cmp
			~x1:expUniqueLst
			~x2:obsUniqueLst;
		()
		end
	in

	let origSeq1 = [] in
	let expUniques1 = [] in
	checkUniques origSeq1 expUniques1;


	let origSeq2 = [1; 3; 1; 4; 5; 1; 3] in
	let expUniques2 = [1; 3; 4; 5] in
	checkUniques origSeq2 expUniques2;
			

	let origSeq3 = [1; 1; 1; 1; 1;] in
	let expUniques3 = [1] in
	checkUniques origSeq3 expUniques3;

	let origSeq4 = [1; 1; 2; 3; 4;] in
	let expUniques4 = [1; 2; 3; 4] in
	checkUniques origSeq4 expUniques4;
	end)



let testSuite () = begin
	let tests = [
		test1 (); 
		test2 (); 
		test3 (); 
		test4 (); 
		test5 (); 
		test6 (); 
		test7 ();
		test8 ();
		test9 ();
		test10 ();
		test11 ();
		test12 ();
		test13 ();
		test14 ();
		test15 ();
		test16 ();
		test17 ();
		test18 ();
		test19 ();
		test20 ();
		test21 ();
		test22 ();
		test23 ();
		]
	in
	let suite = TestCase.createSuite "Iterator" tests in
	suite
	end

