
type tOutcome =  
	| Success of string 
	| Failure of string
	| IgnoredFailure of string

type tResponse = 
	| Continue 
	| Exit

type tOutcomeHistory = {
	mutable failures: tOutcome list; (** only Failures get saved, saving 
	Successes uses too much memory over long runs *)
	mutable totalOutcomes: int; (** total number of outcomes seen *)
}

let createHistory () = {failures = []; totalOutcomes = 0}


let collectOutcomes = ref false
let globalHistory = createHistory ()
let lastOutcome = ref None

let archiveOutcome outcome = begin
	match (!collectOutcomes) with
	|false -> ()
	|true -> begin
		globalHistory.totalOutcomes <- globalHistory.totalOutcomes + 1;
		match outcome with
		|Failure(_) -> globalHistory.failures <- outcome::(globalHistory.failures)
		|_ -> ()
		end
	end

(** levels to log the declared messages *)
let successLogLevel = ref Log.Spew
let failureLogLevel = ref Log.Crash

let failureResponse = ref Exit

(** a mode that doesn't log calls to declareFailure as failures, but as
successes. for testing expected failures*)
let noFail = ref false
(** this will be true if a failure is declared in the current noFail block
(since the most recent call to startNoFail() *)
let noFailBlockHasFailure = ref false

let declareOutcome outcome = begin
	begin (** log message *)
		match outcome with
		|Success(lbl) -> begin
			let msg = "[Success] " ^ lbl in
			Log.logMessage !successLogLevel msg
			end
		|Failure(lbl) -> begin
			let msg = "[FAILURE] " ^ lbl in
			Log.logMessage !failureLogLevel msg
			end
		|IgnoredFailure(lbl) ->  begin
			let msg = "[IgnoredFailure] " ^ lbl in
			Log.logMessage !failureLogLevel msg
			end
	end;
	lastOutcome := Some(outcome);
	archiveOutcome outcome;
	()
	end

let declareSuccess ~label = begin
	declareOutcome (Success(label))
	end

let declareFailure ~label = begin
	match !noFail with
	|true -> begin
		declareOutcome (IgnoredFailure(label));
		noFailBlockHasFailure := true;
		()
		end
	|false -> begin
		declareOutcome (Failure(label));
		match !failureResponse with
		|Continue -> ()
		|Exit -> failwith "Verification Failed. Exiting"
		end
	end


let startNoFail () = begin
	noFailBlockHasFailure := false;
	noFail := true;
	()
	end

let stopNoFail () = begin
	noFail := false;
	!noFailBlockHasFailure
	end

let setCollectOutcomes shouldCollect = collectOutcomes := shouldCollect

let rec getLastOutcome () = begin
	match !lastOutcome with
	|None -> begin
		declareFailure ("CheckHandler: Trying to get lastOutcome before " ^
				"any verifications");
		getLastOutcome ();
		end
	|Some(outcome) -> outcome
	end

let getFailureHistory () = begin
	match !collectOutcomes with
	|false -> begin 
		Log.warn "CheckHandler: Not collecting outcome history, returning []"; 
		globalHistory.failures
		end
	|true -> globalHistory.failures
	end

let getHistoryCount ()  = begin
	match !collectOutcomes with
	|false -> begin 
		Log.warn "CheckHandler: Not collecting outcome history, 0"; 
		globalHistory.totalOutcomes
		end
	|true -> globalHistory.totalOutcomes

	end

let clearHistory () = begin
	globalHistory.failures <- [];
	globalHistory.totalOutcomes <- 0;
	end

let setFailResponse response logLevel = begin
	failureLogLevel := logLevel;
	failureResponse := response;
	end

let setSuccessResponse logLevel = begin
	successLogLevel := logLevel;
	end
