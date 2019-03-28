(* Defines the string graph structure for string graph tests *)

signature TESTGRAPH = 
sig
	structure TestGraph : GRAPH
end

structure StringTestGraph : TESTGRAPH = 
struct
	structure TestStringKey = 
	struct
		type ord_key = string
		val compare = String.compare
	end

	structure TestStringNode = Node(StringKey)
	structure TestStringEdge = Edge(StringKey)(StringNode)
	structure TestGraph = MapGraph(StringEdge)
end