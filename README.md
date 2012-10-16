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
    
    let compr = Comparer.create
        ~toStringFun:skyObjectToString
        ~compareFun:compareSkyObjects
    in

    let s1 = Sun in
    let s2 = Moon in

    if (Comparer.isGreater compr s1 s2) then begin
        Log.info ("s1 was greater than s2 for values: [s1 = " +
            (Comparer.toString s1) +
            ", s2 = " +
            (Comparer.toString s2) + 
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
