(**
	Unit tests for ArrayUtil

*)



(**  *)
let test1 () = TestCase.createAtomic "modArray" (fun () -> begin
	let targetAry = [|1; 2; 3; 4|] in
	let modFn = fun idx x -> x * idx * 2 in

	let expModifiedAry = [|0; 4; 12; 24|] in
	ArrayUtil.modArray
		~modFun:modFn
		~arr:targetAry;
	
	let cmp = ArrayUtil.makeComparer Comparer.ints in
	Verify.areEqual
		~label:"modified ary, expected ary"
		~cmp:cmp
		~x1:targetAry
		~x2:expModifiedAry;
	()
	end)


(**  *)
let test2 () = TestCase.createAtomic "comparer" (fun () -> begin
	let ary1 = [|1; 2; 3; 4|] in
	let ary2 = [|1; 2; 3; 4; 5|] in
	let ary3 = [|2; 1|] in
	let cmp = ArrayUtil.makeComparer Comparer.ints in

	Verify.areEqual
		~label:"ary1, ary1"
		~cmp:cmp
		~x1:ary1
		~x2:ary1;

	Verify.gt
		~label:"ary2, ary1"
		~cmp:cmp
		~x1:ary2
		~x2:ary1;

	Verify.gte
		~label:"ary2, ary1"
		~cmp:cmp
		~x1:ary2
		~x2:ary1;

	Verify.lt
		~label:"ary2, ary3"
		~cmp:cmp
		~x1:ary2
		~x2:ary3;

	Verify.lte
		~label:"ary2, ary3"
		~cmp:cmp
		~x1:ary2
		~x2:ary3;

	Verify.notEqual
		~label:"ary1, ary3"
		~cmp:cmp
		~x1:ary1
		~x2:ary2;
	()
	end)

(**  *)
let test3 () = TestCase.createAtomic "sort" (fun () -> begin
	let unsortedAry =  [|1; 4; 5; 2; 3|] in
	let unsortedAryCopy =  [|1; 4; 5; 2; 3|] in

	let expSortedAry = [|1; 2; 3; 4; 5|] in

	let cmp = ArrayUtil.makeComparer Comparer.ints in
	let obsSortedAry = ArrayUtil.sortCopy unsortedAry Comparer.ints in
	Verify.areEqual
		~label:"obsSortedAry, expSortedAry"
		~cmp:cmp
		~x1:obsSortedAry
		~x2:expSortedAry;

	Verify.areEqual
		~label:"(original unsortedARy, unsortedAryCopy): is raw ary untouched?"
		~cmp:cmp
		~x1:unsortedAry
		~x2:unsortedAryCopy;

	ArrayUtil.sort unsortedAry Comparer.ints;
	Verify.areEqual
		~label:"original ary after in place sort, expSortedAry"
		~cmp:cmp
		~x1:unsortedAry
		~x2:expSortedAry;
	()
	end)

(**  
let test4 = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test5 = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test6 = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test7 = TestCase.createAtomic "" (fun () -> begin
	end)
*)

let testSuite () = begin
	let tests = [
		test1 (); 
		test2 (); 
		test3 (); 
		(*
		test4; 
		test5; 
		test6; 
		test7*)
		]
	in
	let suite = TestCase.createSuite "ArrayUtil" tests in
	suite
	end


