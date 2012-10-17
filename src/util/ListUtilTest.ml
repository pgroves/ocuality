(**
	Unit tests for ListUtil 

*)



(**  *)
let test1 () = TestCase.createAtomic "comparer" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [1; 2; 3; 4; 5] in
	let lst3 = [2; 1] in
	let cmp = ListUtil.makeComparer Comparer.ints in

	Verify.areEqual
		~label:"lst1, lst1"
		~cmp:cmp
		~x1:lst1
		~x2:lst1;

	Verify.gt
		~label:"lst2, lst1"
		~cmp:cmp
		~x1:lst2
		~x2:lst1;

	Verify.gte
		~label:"lst2, lst1"
		~cmp:cmp
		~x1:lst2
		~x2:lst1;

	Verify.lt
		~label:"lst2, lst3"
		~cmp:cmp
		~x1:lst2
		~x2:lst3;

	Verify.lte
		~label:"lst2, lst3"
		~cmp:cmp
		~x1:lst2
		~x2:lst3;

	Verify.notEqual
		~label:"lst1, lst3"
		~cmp:cmp
		~x1:lst1
		~x2:lst2;
	()

	end)
(**  *)
let test2 () = TestCase.createAtomic "init" (fun () -> begin
	let genFn = fun () -> 2 in
	let obsLst = ListUtil.init ~genFun:genFn ~length:3 in
	let expLst = [2; 2; 2] in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs gen list"
		~cmp:cmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

(**  *)
let test3 () = TestCase.createAtomic "initIdx" (fun () -> begin
	let genFn = fun idx -> idx * 2 in
	let obsLst = ListUtil.initIdx ~genFun:genFn ~length:3 in
	let expLst = [0; 2; 4] in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"exp, obs gen idx list"
		~cmp:cmp
		~x1:expLst
		~x2:obsLst;
	()

	end)

(**  *)
let test4 () = TestCase.createAtomic "headN" (fun () -> begin
	let cmp = ListUtil.makeComparer Comparer.ints in
	let lst1 = [1; 2; 3; 4] in
	
	let expHead0 = [] in
	let obsHead0 = ListUtil.headN lst1 0 in
	Verify.areEqual
		~label:"expHead0, obsHead0"
		~cmp:cmp
		~x1:expHead0
		~x2:obsHead0;

	let expHead1 = [1; 2] in
	let obsHead1 = ListUtil.headN lst1 2 in
	Verify.areEqual
		~label:"expHead1, obsHead1"
		~cmp:cmp
		~x1:expHead1
		~x2:obsHead1;

	end)

(**  *)
let test5 () = TestCase.createAtomic "splitN" (fun () -> begin
	let cmp = ListUtil.makeComparer Comparer.ints in
	let lst1 = [1; 2; 3; 4] in
	
	let expHead0 = [] in
	let obsHead0, obsTail0 = ListUtil.splitN lst1 0 in
	Verify.areEqual
		~label:"expHead0, obsHead0"
		~cmp:cmp
		~x1:expHead0
		~x2:obsHead0;
	Verify.areEqual
		~label:"expHead0, obsHead0"
		~cmp:cmp
		~x1:lst1
		~x2:obsTail0;


	let expHead1 = [1; 2] in
	let expTail1 = [3; 4] in
	let obsHead1, obsTail1 = ListUtil.splitN lst1 2 in
	Verify.areEqual
		~label:"expHead1, obsHead1"
		~cmp:cmp
		~x1:expHead1
		~x2:obsHead1;

	Verify.areEqual
		~label:"expTail1, obsTail1"
		~cmp:cmp
		~x1:expTail1
		~x2:obsTail1;
	()
	end)

(**  *)
let test6 ()  = TestCase.createAtomic "foreachidx" (fun () -> begin
	let tally = ref 0 in
	let iterFn = fun idx elem -> tally := !tally + idx + elem in
	let lst = [1; 2; 5; 9] in
	ListUtil.forEachIdx lst ~iterFun:iterFn;
	let obsSum = !tally in
	let expSum = 23 in
	Verify.areEqual
		~label:"expSum, obsSum"
		~cmp:Comparer.ints
		~x1:expSum
		~x2:obsSum;
	()
	end)

(**  *)
let test7 () = TestCase.createAtomic "mapIdx" (fun () -> begin
	let lst = [1; 2; 3;] in
	let mapFn = fun idx elem -> idx * elem in
	let expLst = [0; 2; 6] in
	let obsLst = ListUtil.mapIdx lst ~mapFun:mapFn in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"explst, obsList"
		~cmp:cmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

let test8 () = TestCase.createAtomic "sort" (fun () -> begin
	let unsortedLst = [2; 1; 9] in
	let expLst = [1; 2; 9] in
	let obsLst = ListUtil.sort unsortedLst Comparer.ints in
	let cmp = ListUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"expLst, obsLst"
		~cmp:cmp
		~x1:expLst
		~x2:obsLst;
	()
	end)

let test9 () = TestCase.createAtomic "makeCrossProduct2" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [20; 30; 40] in
	let pairCmp = Comparer.makePair Comparer.ints Comparer.ints in
	let lstPairCmp = ListUtil.makeComparer pairCmp in
	let expXProd = [
		1, 20;
		1, 30;
		1, 40;
		2, 20;
		2, 30;
		2, 40;
		3, 20;
		3, 30;
		3, 40;
		4, 20;
		4, 30;
		4, 40;
		]
	in
	let obsXProd = ListUtil.makeCrossProduct2 lst1 lst2 in
	Verify.areEqual
		~label:"exp, obs crossproduct"
		~cmp:lstPairCmp
		~x1:expXProd
		~x2:obsXProd;
	()
	end)

let test10 () = TestCase.createAtomic "makeCrossProduct3" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [20; 30;] in
	let lst3 = [55; 66;] in
	let tupleCmp = Comparer.makeTriple Comparer.ints Comparer.ints Comparer.ints in
	let lstCmp = ListUtil.makeComparer tupleCmp in
	let expXProd = [
		1, 20, 55;
		1, 20, 66;
		1, 30, 55;
		1, 30, 66;
		2, 20, 55;
		2, 20, 66;
		2, 30, 55;
		2, 30, 66;
		3, 20, 55;
		3, 20, 66;
		3, 30, 55;
		3, 30, 66;
		4, 20, 55;
		4, 20, 66;
		4, 30, 55;
		4, 30, 66;
		]
	in
	let obsXProd = ListUtil.makeCrossProduct3 lst1 lst2 lst3 in
	Verify.areEqual
		~label:"exp, obs crossproduct"
		~cmp:lstCmp
		~x1:expXProd
		~x2:obsXProd;
	()
	end)

let test11 () = TestCase.createAtomic "makeCrossProduct4" (fun () -> begin
	let lst1 = [1; 2; 3;] in
	let lst2 = [20; 30;] in
	let lst3 = [55; 66;] in
	let lst4 = [75; 85;] in
	let tupleCmp = Comparer.makeQuad 
		Comparer.ints Comparer.ints Comparer.ints Comparer.ints
	in
	let lstCmp = ListUtil.makeComparer tupleCmp in
	let expXProd = [
		1, 20, 55, 75;
		1, 20, 55, 85;
		1, 20, 66, 75;
		1, 20, 66, 85;
		1, 30, 55, 75;
		1, 30, 55, 85;
		1, 30, 66, 75;
		1, 30, 66, 85;
		2, 20, 55, 75;
		2, 20, 55, 85;
		2, 20, 66, 75;
		2, 20, 66, 85;
		2, 30, 55, 75;
		2, 30, 55, 85;
		2, 30, 66, 75;
		2, 30, 66, 85;
		3, 20, 55, 75;
		3, 20, 55, 85;
		3, 20, 66, 75;
		3, 20, 66, 85;
		3, 30, 55, 75;
		3, 30, 55, 85;
		3, 30, 66, 75;
		3, 30, 66, 85;

		]
	in
	let obsXProd = ListUtil.makeCrossProduct4 lst1 lst2 lst3 lst4 in
	Verify.areEqual
		~label:"exp, obs crossproduct"
		~cmp:lstCmp
		~x1:expXProd
		~x2:obsXProd;
	()
	end)


let test12 () = TestCase.createAtomic "fleshOutCombine2" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [20; 30; 40] in
	let pairCmp = Comparer.makePair Comparer.ints Comparer.ints in
	let lstPairCmp = ListUtil.makeComparer pairCmp in
	let expXProd = [
		1, 20;
		2, 30;
		3, 40;
		4, 20;
		]
	in
	let obsXProd = ListUtil.fleshOutCombine2 lst1 lst2 in
	Verify.areEqual
		~label:"exp, obs fleshOut"
		~cmp:lstPairCmp
		~x1:expXProd
		~x2:obsXProd;


	let obsXProdRev = ListUtil.fleshOutCombine2 lst2 lst1 in	
	let expXProdRev = [
		20, 1;
		30, 2;
		40, 3;
		20, 4;
		]
	in

	Verify.areEqual
		~label:"exp, obs fleshOut rev"
		~cmp:lstPairCmp
		~x1:expXProdRev
		~x2:obsXProdRev;
	()
	end)

let test13 () = TestCase.createAtomic "fleshOutCombine3" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [20; 30; 40] in
	let lst3 = [55; 66;] in
	let tupleCmp = Comparer.makeTriple Comparer.ints Comparer.ints Comparer.ints in
	let lstCmp = ListUtil.makeComparer tupleCmp in

	let expXProd = [
		1, 20, 55;
		2, 30, 66;
		3, 40, 55;
		4, 20, 66;
		]
	in
	let obsXProd = ListUtil.fleshOutCombine3 lst1 lst2 lst3 in
	Verify.areEqual
		~label:"exp, obs fleshOut"
		~cmp:lstCmp
		~x1:expXProd
		~x2:obsXProd;


	let obsXProdRev = ListUtil.fleshOutCombine3 lst2 lst1 lst3 in	
	let expXProdRev = [
		20, 1, 55;
		30, 2, 66;
		40, 3, 55;
		20, 4, 66;
		]
	in

	Verify.areEqual
		~label:"exp, obs fleshOut rev"
		~cmp:lstCmp
		~x1:expXProdRev
		~x2:obsXProdRev;
	()
	end)

let test14 () = TestCase.createAtomic "fleshOutCombine4" (fun () -> begin
	let lst1 = [1; 2; 3; 4] in
	let lst2 = [20; 30; 40] in
	let lst3 = [55; 66;] in
	let lst4 = [75; 85; 95; 105; 115] in
	let tupleCmp = Comparer.makeQuad 
		Comparer.ints Comparer.ints Comparer.ints Comparer.ints
	in
	let lstCmp = ListUtil.makeComparer tupleCmp in

	let expXProd = [
		1, 20, 55, 75;
		2, 30, 66, 85;
		3, 40, 55, 95;
		4, 20, 66, 105;
		1, 30, 55, 115;
		]
	in
	let obsXProd = ListUtil.fleshOutCombine4 lst1 lst2 lst3 lst4 in
	Verify.areEqual
		~label:"exp, obs fleshOut"
		~cmp:lstCmp
		~x1:expXProd
		~x2:obsXProd;


	let obsXProdRev = ListUtil.fleshOutCombine4 lst2 lst1 lst4 lst3 in	
	let expXProdRev = [
		20, 1, 75, 55;
		30, 2, 85, 66;
		40, 3, 95, 55;
		20, 4, 105, 66;
		30, 1, 115, 55;
		]
	in

	Verify.areEqual
		~label:"exp, obs fleshOut rev"
		~cmp:lstCmp
		~x1:expXProdRev
		~x2:obsXProdRev;
	()
	end)

(**  *)
let test15 () = TestCase.createAtomic "findMin" (fun () -> begin
	let lst = [1; 2; 0; 4] in
	let obsMin = ListUtil.findMin lst Comparer.ints in
	let expMin = 0 in
	Verify.areEqual
		~label:"exp, obs min"
		~cmp:Comparer.ints
		~x1:expMin
		~x2:obsMin;
	Verify.doesFail
		~label:"finding min of empty list"
		~block:(fun () -> ignore (ListUtil.findMin [] Comparer.ints));
	()
	end)

(**  *)
let test16 () = TestCase.createAtomic "findMax" (fun () -> begin
	let lst = [1; 2; 0; 4] in
	let obsMax = ListUtil.findMax lst Comparer.ints in
	let expMax = 4 in
	Verify.areEqual
		~label:"exp, obs max"
		~cmp:Comparer.ints
		~x1:expMax
		~x2:obsMax;
	Verify.doesFail
		~label:"finding max of empty list"
		~block:(fun () -> ignore (ListUtil.findMax [] Comparer.ints));
	()
	end)

let test17 () = TestCase.createAtomic "makeCrossProduct" (fun () -> begin
	let lst1 = [1; 2; 3;] in
	let lst2 = [20; 30;] in
	let lst3 = [55; 66;] in
	let lst4 = [75; 85;] in
	let lstCmp = ListUtil.makeComparer (ListUtil.makeComparer Comparer.ints) in
	let expXProd = [
		[1; 20; 55; 75];
		[1; 20; 55; 85];
		[1; 20; 66; 75];
		[1; 20; 66; 85];
		[1; 30; 55; 75];
		[1; 30; 55; 85];
		[1; 30; 66; 75];
		[1; 30; 66; 85];
		[2; 20; 55; 75];
		[2; 20; 55; 85];
		[2; 20; 66; 75];
		[2; 20; 66; 85];
		[2; 30; 55; 75];
		[2; 30; 55; 85];
		[2; 30; 66; 75];
		[2; 30; 66; 85];
		[3; 20; 55; 75];
		[3; 20; 55; 85];
		[3; 20; 66; 75];
		[3; 20; 66; 85];
		[3; 30; 55; 75];
		[3; 30; 55; 85];
		[3; 30; 66; 75];
		[3; 30; 66; 85];
		]
	in
	let obsXProd = ListUtil.makeCrossProduct [lst1; lst2; lst3; lst4] in
	Verify.areEqual
		~label:"exp, obs crossproduct"
		~cmp:lstCmp
		~x1:expXProd
		~x2:obsXProd;
	()
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
		]
	in
	let suite = TestCase.createSuite "ListUtil" tests in
	suite
	end

