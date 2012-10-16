
(* compareable methods, using Comparer.ints as a demo *)
let test1 () = TestCase.createAtomic "Compareables" (fun () -> begin
	(*igt*)
	Verify.doesFail
		~label:"Test5CaseA1 should fail"
		~block:(fun () -> Verify.gt ~label:"Test5CaseA1" 
			~cmp:Comparer.ints ~x1:0 ~x2:1);
	Verify.gt ~label:"Test5CaseA2" 
			~cmp:Comparer.ints  ~x1:1 ~x2:0;
	Verify.doesFail
		~label:"Test5CaseA3 should fail"
		~block:(fun () -> Verify.gt ~label:"Test5CaseA3" 
			~cmp:Comparer.ints ~x1:0 ~x2:0) ;

	
	(*ilt*)
	Verify.doesFail
		~label:"Test1CaseB1"
		~block: (fun () -> Verify.lt ~label:"Test1CaseB1"
			~cmp:Comparer.ints ~x1:1 ~x2:0);
	Verify.lt ~label:"Test1CaseB2" 
			~cmp:Comparer.ints ~x1:0 ~x2:1;
	Verify.doesFail
		~label:"Test1CaseB3"
		~block:(fun () -> Verify.lt ~label:"Test1CaseB3"
			~cmp:Comparer.ints  ~x1:0 ~x2:0);

	(*igte*)
	Verify.doesFail
		~label:"TestCaseC1"
		~block:(fun () -> Verify.gte ~label:"Test1CaseC1"
			~cmp:Comparer.ints  ~x1:0 ~x2:1);
	Verify.gte ~label:"Test1CaseC2" 
			~cmp:Comparer.ints ~x1:1 ~x2:0;
	Verify.gte ~label:"Test1CaseC3" 
			~cmp:Comparer.ints ~x1:0 ~x2:0;
	
	(*ilte*)
	Verify.doesFail
		~label:"Test1CaseD1"
		~block:(fun () -> Verify.lte ~label:"Test1CaseD1" 
			~cmp:Comparer.ints ~x1:1 ~x2:0);
	Verify.lte ~label:"Test1CaseD2" 
			~cmp:Comparer.ints ~x1:0 ~x2:1;
	Verify.lte ~label:"Test1CaseD3" 
			~cmp:Comparer.ints ~x1:0 ~x2:0;

	(*iEquals*)
	Verify.doesFail
		~label:"TestCaseE1"
		~block:(fun () -> Verify.areEqual ~label:"Test1CaseE1"
			~cmp:Comparer.ints  ~x1:1 ~x2:0);
	Verify.areEqual ~label:"Test1CaseE2" 
			~cmp:Comparer.ints ~x1:0 ~x2:0;

	end)


let test2 () = TestCase.createAtomic "ListVerifiers" (fun () -> begin
	(*listLength*)
	let iEquals = fun ~label ~x1 ~x2 -> begin
		Verify.areEqual ~label:label ~cmp:Comparer.ints ~x1:x1 ~x2:x2
		end
	in
	Verify.listLength "Test4A1" iEquals 3 [1; 1; 1];
	Verify.doesFail
		~label:"Test4A2"
		~block: (fun () -> Verify.listLength "Test4A2" iEquals 3 [1; 1]);

	(*listEndsWith*)
	Verify.listEndsWith "Test4D1" [0; 1; 2] [1; 2];
	Verify.doesFail
		~label:"Test4D2"
		~block:  (fun () -> Verify.listEndsWith "Test4D2" [0; 1; 2] [1; 3]);
	Verify.listEndsWith "Test4D3" [] [];
	Verify.doesFail
		~label:"Test4D4"
		~block:  (fun () -> Verify.listEndsWith "Test4D4" [0] [1; 3]);
	Verify.doesFail
		~label:"Test4D5"
		~block: (fun () -> Verify.listEndsWith "Test4D5" [0; 1; 2] [1; 0; 1; 2]);
    end)



let testSuite () = begin
	let tests = [
		test1 ();
		test2 ();
		]
	in
	let suite = TestCase.createSuite "Verify" tests in
	suite
	end

