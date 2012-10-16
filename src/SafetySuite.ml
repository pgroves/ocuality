
let testSuite () = begin
	let children = [
		ComparerTest.testSuite ();
		TestCaseTest.testSuite ();
		VerifyTest.testSuite ();
		
		]
	in
	let suite = TestCase.createSuite ~label:"Safety" ~children:children in
	suite
	end

let _ = begin

	Log.setRunLevel Log.Debug;
	CheckHandler.setFailResponse CheckHandler.Continue Log.Warn;
	TestCase.setGlobalPrintStackTrace false;

	let tests = testSuite () in
	ignore (TestCase.run tests);

	end
