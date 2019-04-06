
(* Example graph generator *)
(* These graphs are intended to be used by testcases to avoid duplicated code *)
structure UndirectedConnected : GRAPHGEN = 
struct
	type nodetype = int
	type edge_type = int
	structure TestGraph = StringTestGraph.TestGraph

  val node1 = TestGraph.Edge.Node.gen(10, "node1")
  val node2 = TestGraph.Edge.Node.gen(3, "node2")
  val node3 = TestGraph.Edge.Node.gen(~6, "node6")
	
  val nodes = [
    node1,
    node2, 
    node3
	]

  val edges = [
    TestGraph.Edge.genUndirected(node1, node2, 15, "edge1"),
    TestGraph.Edge.genUndirected(node2, node3, 24, "edge2"),
    TestGraph.Edge.genUndirected(node1, node3, ~18, "edge3")
  ]

	val genGraph = 
  let
    val graphWithoutEdges = foldl (fn(node, graph) => TestGraph.addNode(graph, node)) TestGraph.empty nodes
  in
    foldl (fn(edge, graph) => TestGraph.addEdge(graph, edge)) graphWithoutEdges edges
  end
end