type 'a t = {
	compareFun: ('a -> 'a -> int);
	toStringFun: ('a -> string);
	}

let create ~compareFun ~toStringFun = begin
	let newSelf:'a t = {
		compareFun = compareFun;
		toStringFun = toStringFun;
		}
	in
	newSelf
	end

let makeWith self ~compareFun ~toStringFun = begin
	let cmpFn = begin
		match compareFun with
		|None -> self.compareFun
		|Some(cf) -> cf
		end
	in
	let tsFn = begin
		match toStringFun with
		|None -> self.toStringFun
		|Some(tsf) -> tsf
		end
	in
	let newSelf = create cmpFn tsFn in
	newSelf
	end

let toString self elem = self.toStringFun elem
let compare self elem1 elem2 = self.compareFun elem1 elem2

let lessThan self elem1 elem2 = ((compare self elem1 elem2) < 0)
let lt self elem1 elem2 = lessThan self elem1 elem2

let lessThanOrEqual self elem1 elem2 = ((compare self elem1 elem2) <= 0)
let lte self elem1 elem2 = lessThanOrEqual self elem1 elem2

let greaterThan self elem1 elem2 = ((compare self elem1 elem2) > 0)
let gt self elem1 elem2 = greaterThan self elem1 elem2

let greaterThanOrEqual self elem1 elem2 = ((compare self elem1 elem2) >= 0)
let gte self elem1 elem2 = greaterThanOrEqual self elem1 elem2

let areEqual self elem1 elem2 = ((compare self elem1 elem2) = 0)

let notEqual self elem1 elem2 = not (areEqual self elem1 elem2)

let theLessorOf self elem1 elem2 = begin
	if lessThanOrEqual self elem1 elem2 then elem1 else elem2
	end

let theGreaterOf self elem1 elem2 = begin
	if greaterThanOrEqual self elem1 elem2 then elem1 else elem2
	end

let inRange self ~x ~min ~max = begin
	let gteMin = gte self x min in
	let ltMax = lt self x max in
	let inRange = gteMin && ltMax in
	inRange
	end

let ints = create ~compareFun:Pervasives.compare ~toStringFun:string_of_int
let floats = create ~compareFun:Pervasives.compare ~toStringFun:string_of_float
let nums = create ~compareFun:Num.compare_num ~toStringFun:Num.string_of_num
let strings = create ~compareFun:String.compare ~toStringFun:(fun x -> x)

let makeFloatsDelta ~delta = begin

	let compareFn = fun flt1 flt2 -> begin
		match (flt1 < (flt2 -. delta)) with
		|true -> (-1)
		|false -> begin	
			match (flt1 > (flt2 +. delta)) with
			|true -> 1
			|false -> 0
			end
		end
	in
	let deltaStr = " +/-" ^ (string_of_float delta) in
	let toStringFn = fun flt -> begin
		(string_of_float flt) ^ deltaStr
		end
	in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end


let makePair cmpA cmpB = begin
	let compareFn = fun (elemA1, elemB1) (elemA2, elemB2) -> begin
		let cmpByA = compare cmpA elemA1 elemA2 in
		match cmpByA = 0 with
		|false -> cmpByA
		|true -> compare cmpB elemB1 elemB2 
		end
	in
	let toStringFn = fun (elemA, elemB) -> begin
		let strA = toString cmpA elemA in
		let strB = toString cmpB elemB in
		("(" ^ strA ^ ", " ^ strB ^ ")")
		end
	in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end

let makeTriple cmpA cmpB cmpC= begin
	let compareFn = fun (elemA1, elemB1, elemC1) (elemA2, elemB2, elemC2) -> begin
		let cmpByA = compare cmpA elemA1 elemA2 in
		match cmpByA = 0 with
		|false -> cmpByA
		|true -> begin
			let cmpByB = compare cmpB elemB1 elemB2 in
			match cmpByB = 0 with
			|false -> cmpByB
			|true -> compare cmpC elemC1 elemC2
			end
		end
	in
	let toStringFn = fun (elemA, elemB, elemC) -> begin
		let strA = toString cmpA elemA in
		let strB = toString cmpB elemB in
		let strC = toString cmpC elemC in
		("(" ^ strA ^ ", " ^ strB ^ ", " ^ strC ^ ")")
		end
	in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end

let makeQuad cmpA cmpB cmpC cmpD = begin
	let compareFn = fun (elemA1, elemB1, elemC1, elemD1) 
		(elemA2, elemB2, elemC2, elemD2) -> begin
		let cmpByA = compare cmpA elemA1 elemA2 in
		match cmpByA = 0 with
		|false -> cmpByA
		|true -> begin
			let cmpByB = compare cmpB elemB1 elemB2 in
			match cmpByB = 0 with
			|false -> cmpByB
			|true -> begin
				let cmpByC = compare cmpC elemC1 elemC2 in
				match cmpByC = 0 with
				|false -> cmpByC
				|true -> compare cmpD elemD1 elemD2
				end
			end
		end
	in
	let toStringFn = fun (elemA, elemB, elemC, elemD) -> begin
		let strA = toString cmpA elemA in
		let strB = toString cmpB elemB in
		let strC = toString cmpC elemC in
		let strD = toString cmpD elemD in
		("(" ^ strA ^ ", " ^ strB ^ ", " ^ strC ^ ", " ^ strD ^ ")")
		end
	in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end

let makeOption cmpA = begin
	let compareFn = fun elemOpt1 elemOpt2 -> begin
		match elemOpt1, elemOpt2 with
		|None, None -> 0
		|None, Some(_) -> (-1)
		|Some(_), None -> 1
		|Some(elem1), Some(elem2) -> compare cmpA elem1 elem2
		end
	in
	let toStringFn = fun elemOpt -> begin
		match elemOpt with
		|None -> "None"
		|Some(elem) -> "Some(" ^ (toString cmpA elem) ^ ")"
		end
	in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end
		
let map self mapFun = begin
	let compareFn = fun x1 x2 -> compare self (mapFun x1) (mapFun x2) in
	let toStringFn = fun x -> toString self (mapFun x) in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end

let invert self = begin
	let compareFn = fun x1 x2 -> (-1) * (compare self x1 x2) in
	let toStringFn = fun x -> toString self x in
	create ~compareFun:compareFn ~toStringFun:toStringFn
	end
