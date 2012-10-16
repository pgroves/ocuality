type tLevel =
	|Crash
	|Warn
	|Test
	|Info
	|Debug
	|Spew

(**message levels have an inherent ordering such that if the logger is at
a given level, all "greater" messages should also be accepted*)
let getLevelPrecedence lvl =
	match lvl with
	|Crash -> 1
	|Warn -> 2
	|Test -> 3
	|Info -> 4
	|Debug -> 5
	|Spew -> 6

(**global run level *)
let runLevelPrecedence = ref (getLevelPrecedence Info)

(**acceptMessageLevel returns a boolean telling if a message with the given level
should be printed or ignored.*)
let acceptMessageLevel lvl =
	((getLevelPrecedence lvl) <= (!runLevelPrecedence))

let levelToString lvl =
	match lvl with
	|Crash -> "CRASH"
	|Warn -> "WARN"
	|Test -> "TEST" 
	|Info -> "INFO"
	|Debug -> "DEBG"
	|Spew -> "SPEW" 



(** reuseable string buffer for log message creation *)
let logBuffer = Buffer.create 80


let padTwoDigits intVal = begin
	let origString = string_of_int intVal in
	let origLen = String.length origString in
	match (origLen == 2 )with
	|true -> origString
	|false -> begin
		match origLen == 1 with
		|true -> "0" ^ origString
		|false -> failwith ("Log.padTwoDigits: got an int not 1 or 2 digits")
		end
	end

let appendCurrentTimeStr buf = begin
	let unixTime = Unix.gettimeofday () in
	let cl = Unix.gmtime unixTime in
	Buffer.add_string buf (string_of_int (1900 + cl.Unix.tm_year));
	Buffer.add_char buf '/';  
	Buffer.add_string buf (padTwoDigits (cl.Unix.tm_mon + 1));
	Buffer.add_char buf '/'; 
	Buffer.add_string buf (padTwoDigits cl.Unix.tm_mday);
	Buffer.add_char buf '-';
	Buffer.add_string buf (padTwoDigits cl.Unix.tm_hour);
	Buffer.add_char buf ':';
	Buffer.add_string buf (padTwoDigits cl.Unix.tm_min);
	Buffer.add_char buf ':'; 
	Buffer.add_char buf ':'; 
	Buffer.add_string buf (padTwoDigits cl.Unix.tm_sec);
	end

(* does the actual work of printing a log message to standard out *)
let logMessage lvl msg = begin
	if(acceptMessageLevel lvl) then begin
		let lvlStr = levelToString lvl in
		Buffer.clear logBuffer;
		Buffer.add_char logBuffer '[';
		appendCurrentTimeStr logBuffer;
		Buffer.add_string logBuffer "] [";
		Buffer.add_string logBuffer lvlStr;
		Buffer.add_string logBuffer "] ";
		Buffer.add_string logBuffer msg;
		Buffer.add_char logBuffer '\n';

		Buffer.output_buffer stdout logBuffer;
		flush stdout; 
		end
	end

let crash msg = logMessage Crash msg
let warn msg = logMessage Warn msg
let test msg = logMessage Test msg
let info msg = logMessage Info msg
let debug msg = logMessage Debug msg
let spew msg = logMessage Spew msg

let setRunLevel lvl = runLevelPrecedence := (getLevelPrecedence lvl)

let flushBuffer () = begin
	flush stdout; 
	Buffer.reset logBuffer
	end

(** this will tell the system to call flushBuffer when the program is exiting
for any reason. this is performed at startup *)
let _ = begin 
	at_exit flushBuffer
	end
