(**
	Unit tests for Comparer 

*)



(**  *)
let test1 () = TestCase.createAtomic "floatsDelta" (fun () -> begin
	let fltCmp = Comparer.makeFloatsDelta ~delta:0.1 in
	Verify.areEqual
		~label:"flts identical"
		~cmp:fltCmp
		~x1:1.1
		~x2:1.2;
	Verify.areEqual
		~label:"flts within delta, first smaller"
		~cmp:fltCmp
		~x1:1.1
		~x2:1.19;
	Verify.areEqual
		~label:"flts within delta, second smaller"
		~cmp:fltCmp
		~x1:1.1
		~x2:1.0001;
	Verify.notEqual
		~label:"floats almost withing delta, first smaller"
		~cmp:fltCmp
		~x1:1.1
		~x2:1.2001;
	Verify.notEqual
		~label:"floats almost withing delta, second smaller"
		~cmp:fltCmp
		~x1:1.1
		~x2:0.99;
	Verify.lt
		~label:"should be less than"
		~cmp:fltCmp
		~x1:1.0
		~x2:1.1001;
	Verify.gt
		~label:"should be greater than"
		~cmp:fltCmp
		~x1:1.2
		~x2:1.009;
	()
	end)
(*
(**  *)
let test2 () = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test3 () = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test4 () = TestCase.createAtomic "" (fun () -> begin
	end)

(**  *)
let test5 () = TestCase.createAtomic "" (fun () -> begin
	end)

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
		(*
		test2 (); 
		test3 (); 
		test4 (); 
		test5 (); 
		test6 (); 
		test7 ();
		*)
		]
	in
	let suite = TestCase.createSuite "Comparer" tests in
	suite
	end

