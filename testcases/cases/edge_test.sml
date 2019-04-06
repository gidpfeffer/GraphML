structure EdgeTest : TESTSUITE = 
struct
	exception Fail
	type prequel_data = (int, int) UndirectedConnected.TestGraph.graph
	type test = prequel_data -> unit -> unit

	fun beforeEach () = UndirectedConnected.genGraph

	fun throwErr errStr () = (print(errStr); raise Fail)

	fun testUndirectedConnected graph () = 
		let
			val numEdges = length (UndirectedConnected.TestGraph.edges graph)
			val _ = Assert.assertEqual(numEdges, 3, throwErr("Wrong number of edges"))
		in
			()
		end

	fun genCases () = [testUndirectedConnected]
end