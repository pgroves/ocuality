
(* List functions *)

let initIdx ~genFun ~length = begin
	let lst = ref [] in
	for i = 0 to (length - 1) do
		lst := (genFun i)::(!lst)
		done;
	List.rev !lst
	end

let init ~genFun ~length = begin
	initIdx (fun i -> ignore i; genFun ()) length
	end

let headN lst count = begin
	let rec loop source n accLst =
		match (n, source) with
		|(0, _) -> accLst 
		|(x, []) -> Verify.crash ~label:"ListUtil.headN: input list not long enough"
		|(x, hd::tl) -> loop tl (x - 1) (hd::accLst)
	in
	let revLst = loop lst count [] in
	List.rev revLst
	end


let splitN lst count =  begin
	let rec loop source n accLst =
		match (n, source) with
		|(0, _) -> accLst, source 
		|(x, []) ->Verify.crash ~label:"ListUtil.splitN: input lst not long enough"
		|(x, hd::tl) -> loop tl (x - 1) (hd::accLst)
	in
	let revLstTop, lstBottom = loop lst count [] in
	((List.rev revLstTop), lstBottom)
	end

let tailN lst count = begin
	let totalLen = List.length lst in
	let chopLen = totalLen - count in
	let _, tail = splitN lst chopLen in
	tail
	end

let makePairs lst = begin
	let accPairLst = ref [] in
	let outerLoop = fun outerElem -> begin
		let innerLoop = fun innerElem -> begin
			accPairLst := (outerElem, innerElem)::!accPairLst;
			()
			end
		in
		List.iter innerLoop lst;
		end
	in
	List.iter outerLoop lst;
	let pairs = List.rev !accPairLst in
	pairs
	end

let forEachIdx lst ~iterFun = begin
	let rec loop idxCounter lst = begin
		match lst with
		|[] -> ()
		|hd::tl -> iterFun idxCounter hd; loop (idxCounter + 1) tl
		end
	in
	loop 0 lst
	end

let mapIdx lst ~mapFun = begin
	let innerMapFun = Block.addIndexClosure mapFun in
	List.map innerMapFun lst
	end

let compare lst1 lst2 elemCmp = begin
	let iter1 = Iterator.fromList lst1 in
	let iter2 = Iterator.fromList lst2 in
	Iterator.compare iter1 iter2 elemCmp
	end

let toString lst elemToString = begin
	let sbuf1 = Buffer.create 100 in
	Buffer.add_string sbuf1 "[";
	let iter = Iterator.fromList lst in
	let addElemStrToBuffer = fun elem -> begin
		let elemStr = elemToString elem in
		Buffer.add_string sbuf1 elemStr;
		match (Iterator.hasNext iter) with
		|true -> Buffer.add_string sbuf1 "; "
		|false -> ()
		end
	in
	Iterator.forEach iter addElemStrToBuffer;
	Buffer.add_string sbuf1 "]";
	let str = Buffer.contents sbuf1 in
	str
	end

let makeComparer elemCmpr = begin
	let compareFn = fun lst1 lst2 -> begin
		compare lst1 lst2 (fun x1 x2 -> Comparer.compare elemCmpr x1 x2)
		end
	in
	let toStringFn = fun lst -> begin
		toString lst (fun elem -> Comparer.toString elemCmpr elem)
		end
	in
	Comparer.create
		~compareFun:compareFn
		~toStringFun:toStringFn
	end

let sort lst cmp = begin
	List.sort (fun x y -> Comparer.compare cmp x y) lst
	end

let findMin lst cmp = begin
	let minSoFar = ref None in
	let visitElem = fun elem -> begin
		match !minSoFar with
		|None -> minSoFar := Some(elem); ()
		|Some(x) -> minSoFar := Some(Comparer.theLessorOf cmp x elem); ()
		end
	in
	List.iter visitElem lst;
	match !minSoFar with
	|None -> Verify.crash ~label:"List.findMin: can't find min of empty list"
	|Some(x) -> x
	end

let findMax lst cmp = begin
	let maxSoFar = ref None in
	let visitElem = fun elem -> begin
		match !maxSoFar with
		|None -> maxSoFar := Some(elem)
		|Some(x) -> maxSoFar := Some(Comparer.theGreaterOf cmp x elem)
		end
	in
	List.iter visitElem lst;
	match !maxSoFar with
	|None -> Verify.crash ~label:"List.findMax: can't find max of empty list"
	|Some(x) -> x
	end

let makeCrossProduct2 lst1 lst2 = begin
	let rec loop lstA lstB accPairLst = begin
		match lstA with
		|[] -> accPairLst 
		|hdA::tlA -> begin
			let rec innerLoop lstBInner accInnerLst = begin
				match lstBInner with
				|[] -> accInnerLst 
				|hdB::tlB -> innerLoop tlB ((hdA, hdB)::accInnerLst)
				end
			in
			let innerPairs = innerLoop lstB [] in
			loop tlA lstB (List.append innerPairs accPairLst)
			end
		end
	in
	let allPairsRev = loop lst1 lst2 [] in
	let allPairs = List.rev allPairsRev in
	allPairs
	end

let makeCrossProduct3 lst1 lst2 lst3 = begin
	let last2CrossProd = makeCrossProduct2 lst2 lst3 in
	let nested3CrossProd = makeCrossProduct2 lst1 last2CrossProd  in
	let nestedToFlatTriple = fun (x1, (x2, x3)) -> x1, x2, x3 in
	let flat3CrossProd = List.map nestedToFlatTriple nested3CrossProd in
	flat3CrossProd
	end

let makeCrossProduct4 lst1 lst2 lst3 lst4 = begin
	let last3CrossProd = makeCrossProduct3 lst2 lst3 lst4 in
	let nested4CrossProd = makeCrossProduct2 lst1 last3CrossProd  in
	let nestedToFlatQuad = fun (x1, (x2, x3, x4)) -> x1, x2, x3, x4 in
	let flat4CrossProd = List.map nestedToFlatQuad nested4CrossProd in
	flat4CrossProd
	end

let makeCrossProduct5 lst1 lst2 lst3 lst4 lst5 = begin
	let last4CrossProd = makeCrossProduct4 lst2 lst3 lst4 lst5 in
	let nested5CrossProd = makeCrossProduct2 lst1 last4CrossProd  in
	let nestedToFlat5 = fun (x1, (x2, x3, x4, x5)) -> x1, x2, x3, x4, x5 in
	let flat5CrossProd = List.map nestedToFlat5 nested5CrossProd in
	flat5CrossProd
	end

let makeCrossProduct6 lst1 lst2 lst3 lst4 lst5 lst6 = begin
	let last5CrossProd = makeCrossProduct5 lst2 lst3 lst4 lst5 lst6 in
	let nested6CrossProd = makeCrossProduct2 lst1 last5CrossProd  in
	let nestedToFlat6 = fun (x1, (x2, x3, x4, x5, x6)) -> x1, x2, x3, x4, x5, x6 in
	let flat6CrossProd = List.map nestedToFlat6 nested6CrossProd in
	flat6CrossProd
	end

let makeCrossProduct7 lst1 lst2 lst3 lst4 lst5 lst6 lst7 = begin
	let last6CrossProd = makeCrossProduct6 lst2 lst3 lst4 lst5 lst6 lst7 in
	let nested7CrossProd = makeCrossProduct2 lst1 last6CrossProd  in
	let nestedToFlat7 = 
		fun (x1, (x2, x3, x4, x5, x6, x7)) -> x1, x2, x3, x4, x5, x6, x7 in
	let flat7CrossProd = List.map nestedToFlat7 nested7CrossProd in
	flat7CrossProd
	end

let makeCrossProduct8 lst1 lst2 lst3 lst4 lst5 lst6 lst7 lst8 = begin
	let last7CrossProd = makeCrossProduct7 lst2 lst3 lst4 lst5 lst6 lst7 lst8 in
	let nested8CrossProd = makeCrossProduct2 lst1 last7CrossProd  in
	let nestedToFlat8 = 
		fun (x1, (x2, x3, x4, x5, x6, x7, x8)) -> x1, x2, x3, x4, x5, x6, x7, x8 in
	let flat8CrossProd = List.map nestedToFlat8 nested8CrossProd in
	flat8CrossProd
	end

let rec makeCrossProduct sourceLstLst = begin
	match sourceLstLst with
	|[] -> []
	|lastList::[] -> List.map (fun x -> [x]) lastList
	|topUnusedLst::tlUnusedLstLst -> begin
		let crossProductOfTl = makeCrossProduct tlUnusedLstLst in
		let nestedCrossProd = makeCrossProduct2 topUnusedLst crossProductOfTl in
		let nestedToFlat = fun (hd, tailList) -> hd::tailList in
		let flatCrossProd = List.map nestedToFlat nestedCrossProd in
		flatCrossProd
		end
	end


(** makes a lst into one target len. If the list isn't targetLen long in the
first place, keeps adding the first elems of the list to the end until it
is long enough. *)
let rec fleshOut lst targetLen = begin
	match List.length lst >= targetLen with
	|true -> headN lst targetLen
	|false -> fleshOut (List.append lst lst) targetLen
	end

let fleshOutCombine2 lst1 lst2 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let maxLen = findMax [len1; len2] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let combinedLst = List.combine fleshedOut1 fleshedOut2 in
	combinedLst
	end

let fleshOutCombine3 lst1 lst2 lst3 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let maxLen = findMax [len1; len2; len3] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let nested123 = List.combine combined12 fleshedOut3 in
	let nestedToFlat = fun ((x1, x2), x3) -> x1, x2, x3 in
	let flattened123 = List.map nestedToFlat nested123 in
	flattened123
	end

let fleshOutCombine4 lst1 lst2 lst3 lst4 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let len4 = List.length lst4 in
	let maxLen = findMax [len1; len2; len3; len4] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let fleshedOut4 = fleshOut lst4 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let combined34 = List.combine fleshedOut3 fleshedOut4 in
	let nested1234 = List.combine combined12 combined34 in
	let nestedToFlat = fun ((x1, x2), (x3, x4)) -> x1, x2, x3, x4 in
	let flattened1234 = List.map nestedToFlat nested1234 in
	flattened1234
	end

let fleshOutCombine5 lst1 lst2 lst3 lst4 lst5 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let len4 = List.length lst4 in
	let len5 = List.length lst5 in
	let maxLen = findMax [len1; len2; len3; len4; len5] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let fleshedOut4 = fleshOut lst4 maxLen in
	let fleshedOut5 = fleshOut lst5 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let combined34 = List.combine fleshedOut3 fleshedOut4 in
	let nested1234 = List.combine combined12 combined34 in
	let nested12345 = List.combine nested1234 fleshedOut5 in
	let nestedToFlat = fun (((x1, x2), (x3, x4)), x5) -> x1, x2, x3, x4, x5 in
	let flattened12345 = List.map nestedToFlat nested12345 in
	flattened12345
	end

let fleshOutCombine6 lst1 lst2 lst3 lst4 lst5 lst6 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let len4 = List.length lst4 in
	let len5 = List.length lst5 in
	let len6 = List.length lst6 in
	let maxLen = findMax [len1; len2; len3; len4; len5; len6] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let fleshedOut4 = fleshOut lst4 maxLen in
	let fleshedOut5 = fleshOut lst5 maxLen in
	let fleshedOut6 = fleshOut lst6 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let combined34 = List.combine fleshedOut3 fleshedOut4 in
	let combined56 = List.combine fleshedOut5 fleshedOut6 in
	let nested1234 = List.combine combined12 combined34 in
	let nested123456 = List.combine nested1234 combined56 in
	let nestedToFlat = fun (((x1, x2), (x3, x4)), (x5, x6)) -> 
		x1, x2, x3, x4, x5, x6
	in
	let flattened123456 = List.map nestedToFlat nested123456 in
	flattened123456
	end

let fleshOutCombine7 lst1 lst2 lst3 lst4 lst5 lst6 lst7 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let len4 = List.length lst4 in
	let len5 = List.length lst5 in
	let len6 = List.length lst6 in
	let len7 = List.length lst7 in
	let maxLen = findMax [len1; len2; len3; len4; len5; len6; len7] Comparer.ints in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let fleshedOut4 = fleshOut lst4 maxLen in
	let fleshedOut5 = fleshOut lst5 maxLen in
	let fleshedOut6 = fleshOut lst6 maxLen in
	let fleshedOut7 = fleshOut lst7 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let combined34 = List.combine fleshedOut3 fleshedOut4 in
	let combined56 = List.combine fleshedOut5 fleshedOut6 in
	let nested1234 = List.combine combined12 combined34 in
	let nested123456 = List.combine nested1234 combined56 in
	let nested1234567 = List.combine nested123456 fleshedOut7 in
	let nestedToFlat = fun ((((x1, x2), (x3, x4)), (x5, x6)), x7) -> 
		x1, x2, x3, x4, x5, x6, x7
	in
	let flattened1234567 = List.map nestedToFlat nested1234567 in
	flattened1234567
	end

let fleshOutCombine8 lst1 lst2 lst3 lst4 lst5 lst6 lst7 lst8 = begin
	let len1 = List.length lst1 in
	let len2 = List.length lst2 in
	let len3 = List.length lst3 in
	let len4 = List.length lst4 in
	let len5 = List.length lst5 in
	let len6 = List.length lst6 in
	let len7 = List.length lst7 in
	let len8 = List.length lst8 in
	let maxLen = findMax 
		[len1; len2; len3; len4; len5; len6; len7; len8] 
		Comparer.ints 
	in
	let fleshedOut1 = fleshOut lst1 maxLen in
	let fleshedOut2 = fleshOut lst2 maxLen in
	let fleshedOut3 = fleshOut lst3 maxLen in
	let fleshedOut4 = fleshOut lst4 maxLen in
	let fleshedOut5 = fleshOut lst5 maxLen in
	let fleshedOut6 = fleshOut lst6 maxLen in
	let fleshedOut7 = fleshOut lst7 maxLen in
	let fleshedOut8 = fleshOut lst8 maxLen in
	let combined12 = List.combine fleshedOut1 fleshedOut2 in
	let combined34 = List.combine fleshedOut3 fleshedOut4 in
	let combined56 = List.combine fleshedOut5 fleshedOut6 in
	let combined78 = List.combine fleshedOut7 fleshedOut8 in
	let nested1234 = List.combine combined12 combined34 in
	let nested123456 = List.combine nested1234 combined56 in
	let nested12345678 = List.combine nested123456 combined78 in
	let nestedToFlat = fun ((((x1, x2), (x3, x4)), (x5, x6)), (x7, x8)) -> 
		x1, x2, x3, x4, x5, x6, x7, x8
	in
	let flattened12345678 = List.map nestedToFlat nested12345678 in
	flattened12345678
	end

