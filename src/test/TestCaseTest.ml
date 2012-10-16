

let verifyIEquals lbl x1 x2 = begin
	Verify.areEqual
		~label:lbl
		~cmp:Comparer.ints
		~x1:x1
		~x2:x2
	end

let test1 () = TestCase.createAtomic "AtomicTestSuccessTest" (fun () -> begin
	let atomicTest = TestCase.createAtomic "InnerSuccessTest" (fun () -> begin
		Verify.success "InnerSuccesTest:startup";
		Verify.success "InnerSuccesTest:finishing";
		end)
	in
	let oTree = TestCase.run atomicTest in
	let verFail, verTotal = TestCase.calcVerificationFailRate oTree in
	verifyIEquals "verFail" 0 verFail;
	verifyIEquals "verTotal" 3 verTotal;
	let atomFail, atomTotal = TestCase.calcAtomFailRate oTree in
	verifyIEquals "atomFail" 0 atomFail;
	verifyIEquals "atomTotal" 1 atomTotal;
	Verify.predicate
		~label:"obs failPaths is []" 
		~pred:([] = (TestCase.findFailPaths oTree));
	()
	end)


let test2 () = TestCase.createAtomic "AtomicTestFailureTest" (fun () -> begin
	let atomicTest = TestCase.createAtomic "InnerFailureTest" (fun () -> begin
		Verify.success "InnerSuccesTest:startup";
		Verify.failure "InnerSuccessTest: problem";
		Verify.success "InnerSuccesTest:finishing";
		end)
	in
	let oTree = TestCase.run atomicTest in
	let verFail, verTotal = TestCase.calcVerificationFailRate oTree in
	verifyIEquals "verFail" 1 verFail;
	verifyIEquals "verTotal" 4 verTotal;
	let atomFail, atomTotal = TestCase.calcAtomFailRate oTree in
	verifyIEquals "atomFail" 1 atomFail;
	verifyIEquals "atomTotal" 1 atomTotal;
	Verify.predicate
		~label:"obs failPaths is NOT []" 
		~pred:(not ([] = (TestCase.findFailPaths oTree)));
	end)


(* test3 produces a semi-complex tree of test suites with failures and verifies
the outcomeTree produced has the correct fail paths *)
let test3 () = TestCase.createAtomic "CheckingFailPaths" (fun () -> begin
    let failingTestFun = fun () -> Verify.failure "a failing test" in
    let succeedingTestFun = fun () -> Verify.success "a succeeding test" in
    let innerTest1Name = "test a" in
    let innerTest1 = TestCase.createAtomic innerTest1Name succeedingTestFun in
    let innerTest2Name =  "test b" in
    let innerTest2 = TestCase.createAtomic innerTest2Name failingTestFun in
    let innerTest3Name =  "test c" in
    let innerTest3 = TestCase.createAtomic innerTest3Name failingTestFun in
    let innerTest4Name =  "test d" in
    let innerTest4 = TestCase.createAtomic innerTest4Name failingTestFun in

    let suite1Name = "suite 1" in
    let suite1 = TestCase.createSuite 
            ~label:suite1Name 
            ~children: [innerTest1; innerTest2]
    in
	let suite2Name = "suite 2" in
    let suite2 = TestCase.createSuite 
            ~label:suite2Name 
            ~children: [innerTest3]
    in
	let suite3Name = "suite 3" in
    let suite3 = TestCase.createSuite 
            ~label:suite3Name 
            ~children: [innerTest4]
    in
	let suite4Name = "nested a" in
    let suite4 = TestCase.createSuite 
            ~label:suite4Name 
            ~children: [suite1; suite2]
    in
	let suite5Name = "nested b" in
    let suite5 = TestCase.createSuite 
            ~label:suite5Name 
            ~children: [suite4; suite3]
    in
	let oTree = TestCase.run suite5 in
	let failPaths = TestCase.findFailPaths oTree in

	let expectedEndsWith1 = [suite5Name; suite4Name; suite1Name; innerTest2Name] 
	in
	let expectedEndsWith2 = [suite5Name; suite4Name; suite2Name; innerTest3Name]
	in
	let expectedEndsWith3 = [suite5Name; suite3Name; innerTest4Name]
	in
	let printer = fun x -> x in
	Verify.areEqual
		~label: "exp, obs Fail Paths List length" 
		~cmp:Comparer.ints
		~x1:3 
		~x2:(List.length failPaths);
	Verify.listEndsWith 
			~label:"failPath1" 
			~printFun:printer
			~x:(List.nth failPaths 2) 
			~endOfX:expectedEndsWith1;
	Verify.listEndsWith 
			~label:"failPath2" 
			~printFun:printer
			~x:(List.nth failPaths 1) 
			~endOfX:expectedEndsWith2;
	Verify.listEndsWith 
			~label:"failPath3" 
			~printFun:printer
			~x:(List.nth failPaths 0) 
			~endOfX:expectedEndsWith3;
	()
    end)

let test4 () = TestCase.createAtomic "ExceptionsAreCaught" (fun () -> begin
	let explodingTestFun = fun () -> failwith "Explosion in Test 4A" in
    let innerTest1Name = "test a" in
    let innerTest1 = TestCase.createAtomic innerTest1Name explodingTestFun in
	let oTree = TestCase.run innerTest1 in
	let failPaths = TestCase.findFailPaths oTree in
	let expectedEndsWith = [innerTest1Name] in
	Verify.areEqual
		~label: "exp, obs Fail Paths List length" 
		~cmp:Comparer.ints
		~x1:1 
		~x2:(List.length failPaths);

	Verify.listEndsWith 
			~label:"failPath1" 
			~printFun:(fun str -> str)
			~x:(List.nth failPaths 0) 
			~endOfX:expectedEndsWith;
	()
    end)


let testSuite () = begin
	let tests =  [
		test1 (); 
		test2 (); 
		test3 (); 
		test4 ();
		]
	in
	let suite = TestCase.createSuite "TestCase" tests in
	suite
	end

