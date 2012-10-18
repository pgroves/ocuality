(*
	This module is an executable that runs a complete test suite of the
	individual module test suites.
*)

let testSuite () = begin
	let children = [
		ComparerTest.testSuite ();
		TestCaseTest.testSuite ();
		VerifyTest.testSuite ();
		IteratorTest.testSuite ();
		ListUtilTest.testSuite ();
		ArrayUtilTest.testSuite ();
		BlockTest.testSuite ();
		
		]
	in
	let suite = TestCase.createSuite ~label:"ocuality" ~children:children in
	suite
	end

let _ = begin

	Log.setRunLevel Log.Debug;
	CheckHandler.setFailResponse CheckHandler.Continue Log.Warn;
	TestCase.setGlobalPrintStackTrace false;

	let tests = testSuite () in
	ignore (TestCase.run tests);

	end
