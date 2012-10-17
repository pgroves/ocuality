#ocuality#

Integrated Logging, Assertion, and Unit Testing library for building quality Ocaml programs.



##Log 

  Global logging that writes to standard out. Multiple levels of
  available, with the ability to set the level to accept and print (plus
  all levels of higher precedence).

  E.g.

  ```ocaml
  let myFunction () = begin
    Log.setRunLevel Log.Info;

    Log.info "This info message will get logged";
    Log.warn ("This warning will show up in the logs, too, it's higher " +
        "precedence than Log.Info");
    Log.debug "This debug statement won't show up. It would only show up " +
        " if Log.setRunLevel was Log.Debug or Log.Spew")
 
    end
   ```

##Comparer

  Encapsulation of  compare() and toString() methods.
  Implementation for primitives included.
  
  ```ocaml
  
  (** the type we want to be able to compare *)
  type tSkyObject =
    |Sun
    |Moon

  (** compare function *)   
  let compareSkyObjects = fun skyObj1 skyObj2 = begin
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

  let main () = begin
    
    let skyComp = Comparer.create
        ~toStringFun:skyObjectToString
        ~compareFun:compareSkyObjects
    in

    let s1 = Sun in
    let s2 = Moon in

    if (Comparer.isGreater skyComp s1 s2) then begin
        Log.info ("s1 was greater than s2 for values: [s1 = " +
            (Comparer.toString skyComp s1) +
            ", s2 = " +
            (Comparer.toString skyComp s2) + 
            "]");
        end;
    end

  end
  ```
  will produce the output:
  ```

  [2012/10/16-20:55::12] [INFO] s1 was greater than s2 for values [s1 = Sun, s2 = Moon] 
  ```

##Verify

 Assertions that use Comparer's when applicable. Produces helpful log
 messages when the verification fails or (optionally) on Success. 

 Continuing with the Sky/Sun/Moon example:

 ```ocaml
    
    let skyComp = Comparer.create
        ~toStringFun:skyObjectToString
        ~compareFun:compareSkyObjects
    in

    (* set the global type of response to create when a verification
    succeeds or fails *) 
    CheckHandler.setSuccessResponse Log.Debug;
    CheckHandler.setFailureReponse CheckHandler.Exit Log.Crash:

    Verify.notEquals
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
    Verify.equals
        ~label:"Are my pairs equal?"
        ~cmp:pairComparer
        ~x1:(Sun, 1) 
        ~x2:(Moon, 2);
```

will produce the following log message, then exit after the Failure occurs.

```
[2012/10/16-20:55::12] [DEBG] [Sucess] I have two sky objects, making sure they aren't the same :: (s1 != s2) for values: (s1 = Sun), (s2 = Moon) 
[2012/10/16-20:55::12] [DEBG] [FAILURE] Are my pairs equal? :: x1 == x2 for values : (x1: (Sun, 1)), (x2 = (Moon, 2))
```


##TestCase
Finally, we can put it all together with a unit testing framework. The
TestCase module makes test out of simple (fun () -> ()) methods, and
inside those methods we can call Verify.* methods to ensure things are
returning correct values. 

Note that the Verify.\* methods can be called in the *application code
or test code*. Calling Verify.\* methods in application code can enforce 
preconditions that must hold whenever that production code runs. This way
you only need to learn one Assertion library, instead of of using the
built in 'assert' method in production code then a separate api from
a unit testing library in test code.

```ocaml

    (** application code that creates a list of the numbers (1..x) *)
    let countToX x = begin

        (** this could fail in either testing or while running 
            the application *)
        Verify.greaterThan
            ~label: "can only count to positive numbers, is x > 0?"
            ~cmp:Comparer.ints
            ~x1: x
            ~x2: 0;

        let rec makeListLoop  accList countdown = begin
            match countdown == 0 with
            |true -> accList
            |false -> makeListLoop (countdown::accList) (countdown - 1)
            end

        let countList = makeListLoop [] x in
        countList
        end

    (** a test case that will exercise our countToX method and make sure it
        outputs some correct values. *)
    let testCase1 = begin
        let testFun () = begin
            Log.test "Starting TestCase1";

            let observedCountTo3 = countToX 3 in
            let expectedCountTo3 = [1; 2; 3];

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
                ~block: (fun () -> countToX (-22));

                
            Log.test "Finished TestCase1";
            end
        in
        let atomicTestCase = TestCase.createAtomic
            ~label:"TestCase1"
            ~testFun:testFun
        in
        atomicTestCase
        end    
    in

    (** now run the test case. Results will be printed to the log (stdout). *)
    ignore (TestCase.run testCase1);

```

will produce the log message:

```


```

