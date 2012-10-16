

type t =
	|Atom of string * (unit -> unit)
	|Suite of string * t list

type tOutcomeTree =
	|AtomOutcome of string * CheckHandler.tOutcome list * int 
		(*lbl, failures, totalVerifications *)
	|SuiteOutcome of string * tOutcomeTree list

type tOutcomePath = string list 

let createAtomic ~label ~testFun = begin
	let lbl = String.lowercase label in
	Atom(lbl, testFun)
	end

let createSuite ~label ~children = begin
	let lbl = String.lowercase label in
	Suite(lbl, children)
	end

let getLabel self = begin
	match self with
	|Atom(lbl, _) -> lbl
	|Suite(lbl, _) -> lbl
	end

let isAtomic self = begin
	match self with
	|Atom(_) -> true
	|Suite(_) -> false
	end

let getChildren self = begin
	match self with
	|Atom(_) -> Verify.crash ~label:"TestCase.getChildren: TestCase is Atomic"
	|Suite(_, children) -> children
	end

(* find the number of verification failures in an outcome tree *)
let calcVerificationFailRate oTree = begin
	(** the number of failures seen overall *)
	let accFails = ref 0 in
	(** the number of verifications seen overall *)
	let accTotal = ref 0 in

	(** traverses the outcome tree, adding to the acc* *)
	let rec loop oTree  = begin
		match oTree with
		|SuiteOutcome(lbl, subTrees) -> List.iter loop subTrees
		|AtomOutcome(lbl, failures, count) -> begin
			accTotal := !accTotal + count;
			accFails := !accFails + (List.length failures);
			()
			end
		end
	in
	loop oTree;
	(!accFails, !accTotal)
	end

(** the number of atomic tests in an outcome tree with at least one failure *)
let calcAtomFailRate oTree = begin

	(** the number of failures seen overall *)
	let accFails = ref 0 in
	(** the number of verifications seen overall *)
	let accTotal = ref 0 in

	(** traverses the outcome tree, adding to the acc* *)
	let rec loop oTree  = begin
		match oTree with
		|SuiteOutcome(lbl, subTrees) -> List.iter loop subTrees
		|AtomOutcome(lbl, failures, count) -> begin
			accTotal := !accTotal + 1;
			let atLeastOneFail = begin
				match failures with
				|[] -> false
				|lst -> true
				end
			in
			if(atLeastOneFail) then (accFails := !accFails + 1) else ();
			end
		end
	in
	loop oTree;
	(!accFails, !accTotal)
	end

(** logs a summary of an outcome tree *)
let logOutcomeSummary oTree = begin
	
	(* find the number of fails/total verification calls *)
	let verFails, verTotal = calcVerificationFailRate oTree in
	(* find if there is at least one failure *)
	let hasFailure = (verFails > 0) in
	let hasFailString = begin
		match hasFailure with
		|true -> "FAILED"
		|false -> "Passed"
		end
	in

	(* get the string rep of the fail rate *)
	let verFailRateStr = begin
		(string_of_int verFails) ^ "/" ^ (string_of_int verTotal) ^
				" Verifications Failed"
		end
	in
	let logMsg = begin
		match oTree with
		|AtomOutcome(lbl, failures, count) -> begin (** test was an atom *)
			"Atom: '" ^ lbl ^ "' " ^ hasFailString ^ ". " ^ verFailRateStr
			end
		|SuiteOutcome(lbl, oTrees) -> begin (*test was a suite *)
			let atomFails, atomTotal = calcAtomFailRate oTree in
			let atomFailRateStr = begin
				(string_of_int atomFails) ^ "/" ^ (string_of_int atomTotal) ^
				" Atomic Tests Failed"
				end
			in
			"Suite: '" ^ lbl ^ "' " ^ hasFailString ^ ". " ^ verFailRateStr ^
			" in " ^ atomFailRateStr
			end
		end
	in
	Log.test logMsg;
	end

let findFailPaths oTree = begin
	let foundFailPaths = ref [] in
	let rec loop oTree parentPath = begin
		match oTree with
		|AtomOutcome(lbl, failures, count) -> begin
			let localPath = lbl :: parentPath in
			let numFails, numTotal = calcVerificationFailRate oTree in
			if (numFails > 0) then begin
				(** the localPath is in reverse order of what we want to return*)
				foundFailPaths := (List.rev localPath) :: !foundFailPaths
				end
			else ();
			end
		|SuiteOutcome(lbl, oTrees) -> begin
			let localPath = lbl :: parentPath in
			let visitTree = fun oTree -> begin
				let numFails, numTotal = calcVerificationFailRate oTree in
				if (numFails > 0) then (loop oTree localPath) else ();
				end
			in
			List.iter visitTree oTrees;
			end
		end
	in
	loop oTree [];
	!foundFailPaths
	end

let abortAndPrintStackTrace = ref false

let setGlobalPrintStackTrace behaviourBool = begin
	abortAndPrintStackTrace := behaviourBool;
	()
	end

let run testCase =  begin

	(* these two are now set in the top level executable 'rallyflag.ml' 
	CheckHandler.setFailResponse CheckHandler.Continue Log.Warn;
	CheckHandler.setSuccessResponse Log.Spew;
	*)
	CheckHandler.setCollectOutcomes true;

	let runAtom = fun label runFun -> begin
		Log.test ("\n\n\t\tAtom: '" ^ label ^ "' Begin.\n\n");
		begin
			try
				runFun ();
				Verify.success ("Atom: '" ^ label ^ "' Finished Properly")
			with _ as exc -> begin
				match !abortAndPrintStackTrace with
				|true -> raise exc 
				|false -> begin
					Verify.failure ("Atom: '" ^ label ^ "' DID NOT FINISH - " ^ 
						(Printexc.to_string exc));
					end
			end
		end;
		Log.test ("Atom: '" ^ label ^ "' End.");
		let failures = CheckHandler.getFailureHistory () in
		let totalVerifications = CheckHandler.getHistoryCount () in
		let tree = AtomOutcome(label, failures, totalVerifications) in
		CheckHandler.clearHistory ();
		logOutcomeSummary tree;
		tree
		end
	in
	let rec runTestCase = fun testCase -> begin
		match testCase with
		|Atom(lbl, atomFun) -> runAtom lbl atomFun
		|Suite(lbl, caseList) -> begin
			Log.test ("Suite: '" ^ lbl ^ "' Begin.");
			let childTrees = List.map runTestCase caseList in
			Log.test ("Suite: '" ^ lbl ^ "' End.");
			let tree = SuiteOutcome(lbl, childTrees) in
			logOutcomeSummary tree;
			tree
			end
		end
	in
	let oTree = runTestCase testCase in
	let failPaths = findFailPaths oTree in
	let visitFailPath = fun failPath -> begin
		let concatPath = fun pathElem acc -> begin
			"/-/" ^ pathElem ^ acc
			end
		in
		let fullPath = List.fold_right concatPath failPath "" in 
		Log.test ("\tFAILED: " ^ fullPath)
		end
	in
	let caseLabel = begin
		match testCase with
		|Atom(lbl, _) -> lbl
		|Suite(lbl, _) -> lbl
		end
	in
	begin
		match failPaths with
		|[] -> Log.test (caseLabel ^ ": All Tests Passed.");
		|_ -> begin
			Log.test (caseLabel ^ ": All Failure Locations:");
			List.iter visitFailPath failPaths;
			end
	end;
	oTree
	end

