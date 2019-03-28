structure EdgeTest : TESTSUITE = 
struct
	exception Fail
	type prequel_data = unit
	type test = unit -> unit -> unit

	fun beforeEach () = ()

	fun testUndirectedConnected () () = 
		let
			val graph = UndirectedConnected.genGraph
			val numEdges = length (UndirectedConnected.TestGraph.edges graph)
		in
			if numEdges = 3 then () else raise Fail
		end

	fun genCases () = [testUndirectedConnected]
end