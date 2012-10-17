
(*******************)
(***Log Example*****)
(*******************)

let logExampleMain() = begin
    Log.setRunLevel Log.Info;

    Log.info "This info message will get logged";
    Log.warn ("This warning will show up in the logs, too, it's higher " ^
        "precedence than Log.Info");
    Log.debug ("This debug statement won't show up. It would only show up " ^
        " if Log.setRunLevel was Log.Debug or Log.Spew")
    end

(********************)
(**Comparer Example**)
(********************)

(** the type we want to be able to compare *)
type tSkyObject =
    |Sun
    |Moon

(** compare function *)   
let compareSkyObjects skyObj1 skyObj2 = begin
    match skyObj1, skyObj2 with
    |Sun, Sun -> 0
    |Sun, Moon -> 1
    |Moon, Sun -> (-1)
    |Moon, Moon -> 0
    end

(** toString function *)   
let skyObjectToString skyObj = begin
    match skyObj with
    |Sun -> "Sun"
    |Moon -> "Moon"
    end

let comparerExampleMain () = begin

    let skyComp = Comparer.create
        ~toStringFun:skyObjectToString
        ~compareFun:compareSkyObjects
    in

    let s1 = Sun in
    let s2 = Moon in

    if (Comparer.greaterThan skyComp s1 s2) then begin
        Log.info ("s1 was greater than s2 for values: [s1 = " ^
            (Comparer.toString skyComp s1) ^
            ", s2 = " ^
            (Comparer.toString skyComp s2) ^ 
            "]");
        end;
    end


(********************)
(***Verify Example***)
(********************)

let verifyExampleMain () = begin

    let skyComp = Comparer.create
        ~toStringFun:skyObjectToString
        ~compareFun:compareSkyObjects
    in

    (* set the global type of response to create when a verification
    succeeds or fails *) 
    CheckHandler.setSuccessResponse Log.Debug;
    CheckHandler.setFailResponse CheckHandler.Continue Log.Crash;

    Verify.notEqual
        ~label: "I have two sky objects, making sure they aren't the same"
        ~cmp: skyComp
        ~x1: Sun
        ~x2: Moon;

    (* Most importantly, we can compose higher order Comparers that
       still work with the Verify.* methods. see Comparer.make*() methods
       for more.
       
       Here, we make a comparer for a pair of type (tSkyObject * int)
       *)


    let pairComparer = Comparer.makePair skyComp Comparer.ints in

    (** this verification is going to fail at runtime *)
    Verify.areEqual
        ~label:"Are my pairs equal?"
        ~cmp:pairComparer
        ~x1:(Sun, 1) 
        ~x2:(Moon, 2);

    end

(***********************)
(***Unit Test Example***)
(***********************)


let testCaseExampleMain () = begin

    (** application code that creates a list of the numbers (1..x) *)
    let countToX x = begin

        (** this could fail in either testing or while running 
            the application *)
        Verify.gt
            ~label: "can only count to positive numbers, is x > 0?"
            ~cmp:Comparer.ints
            ~x1: x
            ~x2: 0;

        let rec makeListLoop  accList countdown = begin
            match countdown == 0 with
            |true -> accList
            |false -> makeListLoop (countdown::accList) (countdown - 1)
            end
        in    

        let countList = makeListLoop [] x in
        countList
        end
    in    

    (** a test case that will exercise our countToX method and make sure it
        outputs some correct values. *)
    let testCase1 = begin
        let testFun () = begin
            Log.test "Starting TestCase1";

            let observedCountTo3 = countToX 3 in
            let expectedCountTo3 = [1; 2; 3] in

            let intListComparer = ListUtil.makeComparer Comparer.ints in
            
            Verify.areEqual
                ~label:"(exp, obs) countTo3"
                ~cmp: intListComparer
                ~x1: expectedCountTo3
                ~x2: observedCountTo3;

            (** Now to get fancy... This will verify that the block of 
                code generates a failure. Note that the failure will
                occur inside the countToX method. *)

            Verify.doesFail    
                ~label:"Passing in a negative number should fail"
                ~block: (fun () -> ignore(countToX (-22)));
                
            Log.test "Finished TestCase1";
            ()
            end
        in
        let atomicTestCase = TestCase.createAtomic
            ~label:"TestCase1"
            ~testFun:testFun
        in
        atomicTestCase
        end    
    in

    CheckHandler.setFailResponse CheckHandler.Exit Log.Crash;
    (** now run the test case. Results will be printed to the log (stdout). *)
    ignore (TestCase.run testCase1);
    end


let _ = begin
    logExampleMain ();
    comparerExampleMain ();
    verifyExampleMain ();
    testCaseExampleMain ();
    end

