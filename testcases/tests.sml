structure Tests :>
sig
  val run : unit -> unit
end
 = 
struct

  fun gen_tests (prequel, cases) = map (fn test => test prequel) cases

  val edge_test_tests = gen_tests(EdgeTest.beforeEach(), EdgeTest.genCases())

  val tests = edge_test_tests

  fun run () = (map (fn test => test()) tests; ())
end