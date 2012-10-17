(*** HOF *)

let addIndexClosure idxAppFun = begin
	let counter = ref 0 in
	let appFun = fun elem -> begin
		let idx = !counter in
		counter := !counter + 1;
		idxAppFun idx elem
		end
	in
	appFun
	end
	
let rec timesDo count fn =
	match count with
		|x when (x = 0) -> ()
		|x -> fn (); timesDo (x - 1) fn

let rec whileDo predicate body = begin
	match (predicate ()) with
	|false -> ()
	|true -> begin
		body ();
		whileDo predicate body;
		end
	end

let rec doWhile predicate body =
	body ();
	if(predicate ()) then (doWhile predicate body)

let forDo maxIdx body = begin
	let rec loop currentIdx maxIdx body = begin
		if(currentIdx < maxIdx) then begin
			body currentIdx;
			loop (currentIdx + 1) maxIdx body
			end
		end
	in
	loop 0 maxIdx body
	end
		
