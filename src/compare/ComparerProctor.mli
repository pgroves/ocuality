(**
	Makes tests for 'a Comparer.t's. all method calls take functions so
	tested structures arent' initialized until the test is running.
*)

type 'a t

val create:
	cmpr:(unit -> 'a Comparer.t) ->
	elemList:(unit -> 'a list) ->
	'a t

val makeReflexiveTest: 'a t -> TestCase.t

val makeTestSuite: 'a t -> TestCase.t



(** for all pairs of elems, verify that a>b => b<a *)
val makeReflexive:
	cmp:(unit -> 'a Comparer.t) ->
	elemList:(unit -> 'a list) ->
	TestCase.t


val makeSuite:
	cmp:(unit -> 'a Comparer.t) ->
	elemList:(unit -> 'a list) ->
	TestCase.t

