

(** given a list of functions that evaluate to [-1; 0; 1], calls them in
order and returns the first non-zero value, or zero if the the end of the 
list is reached and they were all zeros.
*)
let cascadeCompare cmpFunLst = begin
	let cmpFunIter = Iterator.fromList cmpFunLst in
	let cmpResIter = Iterator.map cmpFunIter (fun fn -> fn ()) in
	let nonZeroOpt = Iterator.find cmpResIter (fun cRes -> cRes != 0) in
	match nonZeroOpt with
	|Some(compareResult) -> compareResult
	|None -> 0
	end

let makePickAllCompound1 ~toStringFun ~field1Cmp ~getField1 = begin

	let cmpFn = fun z1 z2 -> begin
		let cmpFn1 () = Comparer.compare field1Cmp (getField1 z1) (getField1 z2) in
		cascadeCompare [cmpFn1]
		end
	in
	let cmpr = Comparer.create
		~compareFun:cmpFn
		~toStringFun:toStringFun
	in
	cmpr
	end


let makePickAllCompound2 ~toStringFun ~field1Cmp ~field2Cmp ~getField1
	~getField2 = begin

	let cmpFn = fun z1 z2 -> begin
		let cmpFn1 () = Comparer.compare field1Cmp (getField1 z1) (getField1 z2) in
		let cmpFn2 () = Comparer.compare field2Cmp (getField2 z1) (getField2 z2) in
		cascadeCompare [cmpFn1; cmpFn2]
		end
	in
	let cmpr = Comparer.create
		~compareFun:cmpFn
		~toStringFun:toStringFun
	in
	cmpr
	end

let makePickAllCompound8 
	~toStringFun 
	~field1Cmp 
	~field2Cmp 
	~field3Cmp 
	~field4Cmp 
	~field5Cmp 
	~field6Cmp 
	~field7Cmp 
	~field8Cmp 
	~getField1
	~getField2 
	~getField3 
	~getField4 
	~getField5 
	~getField6 
	~getField7 
	~getField8 
	= begin

	let cmpFn = fun z1 z2 -> begin
		let cmpFn1 () = Comparer.compare field1Cmp (getField1 z1) (getField1 z2) in
		let cmpFn2 () = Comparer.compare field2Cmp (getField2 z1) (getField2 z2) in
		let cmpFn3 () = Comparer.compare field3Cmp (getField3 z1) (getField3 z2) in
		let cmpFn4 () = Comparer.compare field4Cmp (getField4 z1) (getField4 z2) in
		let cmpFn5 () = Comparer.compare field5Cmp (getField5 z1) (getField5 z2) in
		let cmpFn6 () = Comparer.compare field6Cmp (getField6 z1) (getField6 z2) in
		let cmpFn7 () = Comparer.compare field7Cmp (getField7 z1) (getField7 z2) in
		let cmpFn8 () = Comparer.compare field8Cmp (getField8 z1) (getField8 z2) in
		cascadeCompare [cmpFn1; cmpFn2; cmpFn3; cmpFn4; cmpFn5; cmpFn6; cmpFn7; 
			cmpFn8;]
		end
	in
	let cmpr = Comparer.create
		~compareFun:cmpFn
		~toStringFun:toStringFun
	in
	cmpr
	end


let makePickOneCompound2 
	~toStringFun 
	~field1Cmp 
	~field2Cmp 
	~getField1Opt
	~getField2Opt 
	= begin
	
	let cmpFn = fun z1 z2 -> begin
		let cmpFn1 () = Comparer.compare 
			(Comparer.makeOption field1Cmp) 
			(getField1Opt z1) 
			(getField1Opt z2) 
		in
		let cmpFn2 () = Comparer.compare 
			(Comparer.makeOption field2Cmp)
			(getField2Opt z1) 
			(getField2Opt z2) 
		in
		cascadeCompare [cmpFn1; cmpFn2]
		end
	in
	let cmpr = Comparer.create
		~compareFun:cmpFn
		~toStringFun:toStringFun
	in
	cmpr
	end


let makePickOneCompound8 
	~toStringFun 
	~field1Cmp 
	~field2Cmp 
	~field3Cmp 
	~field4Cmp 
	~field5Cmp 
	~field6Cmp 
	~field7Cmp 
	~field8Cmp 
	~getField1Opt
	~getField2Opt 
	~getField3Opt 
	~getField4Opt 
	~getField5Opt 
	~getField6Opt 
	~getField7Opt 
	~getField8Opt 
	= begin
	
	let cmpFn = fun z1 z2 -> begin
		let cmpFn1 () = Comparer.compare 
			(Comparer.makeOption field1Cmp) 
			(getField1Opt z1) 
			(getField1Opt z2) 
		in
		let cmpFn2 () = Comparer.compare 
			(Comparer.makeOption field2Cmp)
			(getField2Opt z1) 
			(getField2Opt z2) 
		in
		let cmpFn3 () = Comparer.compare 
			(Comparer.makeOption field3Cmp)
			(getField3Opt z1) 
			(getField3Opt z2) 
		in
		let cmpFn4 () = Comparer.compare 
			(Comparer.makeOption field4Cmp)
			(getField4Opt z1) 
			(getField4Opt z2) 
		in
		let cmpFn5 () = Comparer.compare 
			(Comparer.makeOption field5Cmp)
			(getField5Opt z1) 
			(getField5Opt z2) 
		in
		let cmpFn6 () = Comparer.compare 
			(Comparer.makeOption field6Cmp)
			(getField6Opt z1) 
			(getField6Opt z2) 
		in
		let cmpFn7 () = Comparer.compare 
			(Comparer.makeOption field7Cmp)
			(getField7Opt z1) 
			(getField7Opt z2) 
		in
		let cmpFn8 () = Comparer.compare 
			(Comparer.makeOption field8Cmp)
			(getField8Opt z1) 
			(getField8Opt z2) 
		in
		cascadeCompare [cmpFn1; cmpFn2; cmpFn3; cmpFn4; cmpFn5; cmpFn6; cmpFn7;
			cmpFn8]

		end
	in
	let cmpr = Comparer.create
		~compareFun:cmpFn
		~toStringFun:toStringFun
	in
	cmpr
	end


