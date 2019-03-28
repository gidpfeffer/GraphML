signature TESTSUITE = 
sig
	exception Fail
	type prequel_data

	(* Every test takes in data and output nothing, should throw Fail exception in event of failure *)
	type test = prequel_data -> unit -> unit

	(* Initializer which produces data to be fed into every test *)
	val beforeEach: unit -> prequel_data

	(* Returns a list of tests for the suite *)
	val genCases : unit -> test list
end
(*
functor TestGen(Suite: TESTSUITE) :>
	sig
    val genTests : unit -> 
  end*)