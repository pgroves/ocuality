
let modArray ~modFun ~arr = begin
	let lastIdx = (Array.length arr - 1) in
	for i = 0 to lastIdx do
		arr.(i) <- modFun i arr.(i)
		done
	end

let countInstancesInArray arr elem = begin
	let arrLength = Array.length arr in
	let tally = ref 0 in
	let body = fun idx -> begin
		let curElem = Array.get arr idx in
		if(curElem = elem) then (tally := !tally + 1) else ();
		end
	in
	Block.forDo arrLength body;
	!tally
	end


let compare ary1 ary2 elemCmp = begin
	let iter1 = Iterator.fromArray ary1 in
	let iter2 = Iterator.fromArray ary2 in
	Iterator.compare iter1 iter2 elemCmp
	end

let toString ary elemToString = begin
	let sbuf1 = Buffer.create 100 in
	Buffer.clear sbuf1;
	Buffer.add_string sbuf1 "[|";
	let iter = Iterator.fromArray ary in
	let addElemStrToBuffer = fun elem -> begin
		let elemStr = elemToString elem in
		Buffer.add_string sbuf1 elemStr;
		match (Iterator.hasNext iter) with
		|true -> Buffer.add_string sbuf1 "; "
		|false -> ()
		end
	in
	Iterator.forEach iter addElemStrToBuffer;
	Buffer.add_string sbuf1 "|]";
	let str = Buffer.contents sbuf1 in
	Buffer.clear sbuf1;
	str
	end

let makeComparer elemCmpr = begin
	let compareFn = fun ary1 ary2 -> begin
		compare ary1 ary2 (fun x1 x2 -> Comparer.compare elemCmpr x1 x2)
		end
	in
	let toStringFn = fun ary -> begin
		toString ary (fun elem -> Comparer.toString elemCmpr elem)
		end
	in
	Comparer.create
		~compareFun:compareFn
		~toStringFun:toStringFn
	end

let sort ary cmpr = begin
	let cmpFn = fun x1 x2 -> Comparer.compare cmpr x1 x2 in
	Array.sort cmpFn ary
	end

let sortCopy ary cmpr = begin
	let aryCopy = Array.copy ary in
	sort aryCopy cmpr;
	aryCopy
	end

