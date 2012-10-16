
type 'a t = {
	initComparerFun:(unit -> 'a Comparer.t);
	initElemListFun:(unit -> 'a list);
	}

let makeReflexive ~cmp ~elemList = begin

	let reflexTest (elemA, elemB) cmp = begin
		let relationAB = Comparer.compare cmp elemA elemB in
		let relationBA = Comparer.compare cmp elemB elemA in
		let strA = Comparer.toString cmp elemA in
		let strB = Comparer.toString cmp elemB in
		Verify.areEqual
			~label:("is relationAB = (-1) * relationBA? A=" ^ strA ^ ", B=" ^ strB)
			~cmp:Comparer.ints
			~x1:relationAB
			~x2:((-1) * relationBA);
		()
		end
	in
	let testFn = fun () -> begin
		let pairs = ListUtil.makePairs (elemList ()) in
		let cmp = cmp () in
		List.iter (fun pair -> reflexTest pair cmp) pairs;
		end
	in
	let test = TestCase.createAtomic
		~label:"reflexive"
		~testFun:testFn
	in
	test
	end

let makeSuite ~cmp ~elemList = begin
	let tests = [
		makeReflexive ~cmp:cmp ~elemList:elemList;
		]
	in
	let suite = TestCase.createSuite "ComparerProctor" tests in
	suite
	end


let makeTestSuite self = begin
	let suite = makeSuite 
		~cmp:self.initComparerFun 
		~elemList:self.initElemListFun
	in
	suite
	end

let makeReflexiveTest self = begin
	let case = makeReflexive
		~cmp:self.initComparerFun 
		~elemList:self.initElemListFun
	in
	case

	end

let create ~cmpr ~elemList = begin
	let newSelf = {
		initComparerFun = cmpr;
		initElemListFun = elemList;
		}
	in
	newSelf
	end	
