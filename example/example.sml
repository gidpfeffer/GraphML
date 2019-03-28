structure StringToIntGenerator: STRING_GENERATOR = 
struct 
	type gen_type = int
	fun generate str = valOf (Int.fromString str)
end

structure GraphBuilder = BasicGraphBuilder(StringToIntGenerator)(StringToIntGenerator)
structure GAlg = GraphAlg(GraphBuilder.Graph)

fun printBanner () = print("#####################\n");

fun printEdge edge = (
	print("Start: ");
	print(StringNode.genKey (StringEdge.genStart edge));
	print(" End: ");
	print(StringNode.genKey (StringEdge.genFinish edge));
	print("\n")
)

fun printStats (graph, node) = (
  printBanner ();
	print("Successors of node: ");
	print(StringNode.genKey node);
	print("\n\n");
	map printEdge (StringGraph.genOutboundEdges (graph, node));
	printBanner ()
)

fun printGraphStats graph = 
  let
    val nodes = StringGraph.nodes graph
  in
    map (fn n => printStats (graph, n)) nodes
  end

val _ = case GraphBuilder.build "example/basicgraph.txt" of
	NONE => print("INVALID GRAPH FILE\n")
  | SOME(graph) => 
  	let 
  		val startNode = valOf (StringGraph.findNode (graph,"node_key_1"))
  		val finishNode = valOf (StringGraph.findNode (graph,"node_key_4"))
  		val edgefn = (fn edge => Real.fromInt (StringEdge.genVal edge))
  		val path = GAlg.shortestPath (graph, startNode, finishNode) edgefn
  		val _ = case path of
  			NONE => print("NO PATH FOUND\n")
  		  | SOME(p) => (
  		  	let
  		  		val edge_names = map (fn e => StringGraph.Edge.genKey e) p
  		  		val edge_str = String.concatWith ", " edge_names
  		  	in
  		  		(print("PATH FOUND:\n"); print(edge_str ^ "\n"))
  		  	end
  		  )
  		val mstPrint = case GAlg.mst graph edgefn of
  			NONE => print("NO MST!\n")
  		  | SOME(g) => 
  		  	(printBanner ();
  		  	print("MST STATS:\n");
  		  	map printEdge (StringGraph.edges g);
  		  	printBanner ())
      val (graphWithoutStart, edgesTouchingStart) = StringGraph.removeNode (graph, startNode)
      val printRelatedEdges = 
        (
          print("Edges incident to " ^ StringGraph.Edge.Node.genKey startNode ^ ":\n");
          (print(String.concatWith ", " (map (fn e => StringGraph.Edge.genKey e) edgesTouchingStart)));
          print("\n\n NEW GRAPH STATS: \n\n");
          printGraphStats graphWithoutStart
        )
  	in
	  	(
			(if GAlg.negativeCycleExists (graph, startNode) (fn edge => Real.fromInt (StringEdge.genVal edge)) then print("Negative cyle found\n") else print("No negative cyle found\n"));
			()
		)
  	end