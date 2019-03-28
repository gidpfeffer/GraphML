functor Node(K: ORD_KEY) :> NODE where Key = K =
	struct
		structure Key = K
		(* Pretty simple stores a value and a key *)
		datatype 'a node = NODE of 'a * Key.ord_key
		fun gen (value, key) = NODE(value, key)
		fun genVal (NODE(value, key)) = value
		fun genKey (NODE(value, key)) = key
	end

functor Edge(K: ORD_KEY)(N: NODE) :> EDGE where Key = K where Node = N =
struct
	structure Key = K
	structure Node = N

	(* Also simple, tracks start, finish, 'b value, identifying key, and boolean indicating directed *)
	datatype ('a,'b) edge = Edge of 'a N.node * 'a N.node * 'b * Key.ord_key * bool

	fun genDirected (start, finish, value, key) = Edge(start, finish, value, key, true)
	fun genUndirected (start, finish, value, key) = Edge(start, finish, value, key, false)
	fun genVal (Edge(_, _, value, _, _)) = value
	fun genKey (Edge(_, _, _, key, _)) = key
	fun genStart (Edge(start, _, _, _, _)) = start
	fun genFinish (Edge(_, finish, _, _, _)) = finish
	fun genIsDirected (Edge(_, _, _, _, directed)) = directed
end

functor MapGraph (E: EDGE) :> GRAPH where Edge = E = 
struct
	structure Node = E.Node
	structure Edge = E
	structure NMap = RedBlackMapFn(Node.Key)
	structure EMap = RedBlackMapFn(E.Key)
	exception NodeNotFound of string;
	(* A node key in the graph maps to a tuple containing (node, edge map)
		Where edge map is a mapping from edge key to edge *)
	datatype ('a,'b) graph = GRAPH of ((('a Node.node) * ((('a,'b) E.edge) EMap.map)) NMap.map)
	fun flatten xs = List.foldr (fn (x, acc) => x @ acc) [] xs
	val empty = GRAPH(NMap.empty)
	fun nodes (GRAPH(m)) = map (fn (node, edge_map) => node) (NMap.listItems m)
	
	fun edges (GRAPH(m)) = 
		let
			val allEdges = flatten (map (fn (node, edge_map) => EMap.listItems edge_map) (NMap.listItems m))
			val uniqueEdges = foldl (fn (edge, map) => EMap.insert(map, E.genKey edge, edge)) EMap.empty allEdges
		in
			map (fn (k,v) => v) (EMap.listItemsi uniqueEdges)
		end

	(* Ignores calls to add a node which is already present *)
	fun addNode ((GRAPH(m)), node) =
		case NMap.find (m, Node.genKey node) of
			SOME(_) => GRAPH(m)
		  | NONE => GRAPH(NMap.insert (m, Node.genKey node, (node, EMap.empty)))
	(* Ignores calls to add a edge which is already present *)
	fun addEdge ((GRAPH(m)), edge) = 
		let
			val start_key = Node.genKey (E.genStart edge)
			val finish_key = Node.genKey (E.genFinish edge)
			val edge_key = E.genKey edge
			val directed = E.genIsDirected edge
			(* Adds outbound edge to make genOutbound edges simple *)
			fun addEdge (node_key, edge, (GRAPH(m))) = case NMap.find (m, node_key) of
				SOME((node, edge_map)) => (case (EMap.find (edge_map, E.genKey edge)) of
					  (* Dont add duplicates, just like nodes *)
					  SOME(edge) => GRAPH(m)
					  (* Edge wasnt present, so add it *)
					| NONE => GRAPH(NMap.insert (m, node_key, (node, EMap.insert (edge_map, edge_key, edge)))))
				(* Couldnt find node in graph *)
			  | NONE => raise NodeNotFound "Node from edge not present in graph"
			val newGraph = addEdge(start_key, edge, GRAPH(m))
		in
			(* Add both mappings if undirected *)
			if E.genIsDirected edge then newGraph else addEdge(finish_key, edge, newGraph)
		end

	(* Give a node in the graph, it gives all outbound edges *)
	fun genOutboundEdges ((GRAPH(m)), node) = case NMap.find (m, Node.genKey node) of 
		  SOME((node, edge_map)) => EMap.listItems edge_map
		| NONE => raise NodeNotFound "Node to generate successors from not present in graph"

	fun nodeExists ((GRAPH(m)), node) = case NMap.find (m, Node.genKey node) of
		SOME(_) =>  true
	  | NONE => false

	fun edgeExists ((GRAPH(m)), edge) = case NMap.find (m, Node.genKey (E.genStart edge)) of
		SOME((node, edge_map)) => (case (EMap.find (edge_map, E.genKey edge)) of
			  (* Dont add duplicates, just like nodes *)
			  SOME(edge) => true
			  (* Edge wasnt present, so add it *)
			| NONE => false)
		(* Couldnt find node in graph *)
	  | NONE => false


	fun findNode ((GRAPH(m)),key) = case NMap.find(m, key) of
		NONE => NONE
	  |	SOME((node, edge_map)) => SOME(node)

	fun removeEdge ((GRAPH(m)),edge) = 
		let
			val edgeKey = Edge.genKey edge
			val directed = Edge.genIsDirected edge
			val startNode = Edge.genStart edge
			val finishNode = Edge.genFinish edge
			fun removeEdgeFromNode node edge m = case NMap.find(m, Edge.Node.genKey node) of
				NONE => m
			  | SOME((node, edge_map)) => (
			  	let
			  		val (newEdgeMap, edge) = EMap.remove(edge_map, Edge.genKey edge)
			  	in
			  		NMap.insert(m, Edge.Node.genKey node, (node, newEdgeMap))
			  	end
			  )

			val newGraph = removeEdgeFromNode startNode edge m
			val selfEdge = case Edge.Node.Key.compare ((Edge.Node.genKey startNode),(Edge.Node.genKey finishNode)) of
				EQUAL => true
			  | _ => false
		in
			if (directed orelse selfEdge) then GRAPH(newGraph) else GRAPH(removeEdgeFromNode finishNode edge m)
		end

	fun removeNode ((GRAPH(m)),node) = 
		let
			fun edges (node, m) = case NMap.find(m, Edge.Node.genKey node) of
				NONE => []
			  | SOME((node, edge_map)) => EMap.listItems(edge_map)
			val adjacentEdges = edges (node, m)
			val GRAPH(m) = foldl (fn (edge, graph) => removeEdge (graph, edge)) (GRAPH(m)) adjacentEdges
			val (mapWithoutNode, removedNode) = NMap.remove(m, Edge.Node.genKey node)
		in
			(GRAPH(mapWithoutNode), adjacentEdges)
		end

	(* Can turn an old graph into a new graphical representation *)
	fun transformGraph (node_map_fn, edge_map_fn) graph =
		let
			val new_nodes = map node_map_fn (nodes(graph))
			val new_edges = map edge_map_fn (edges(graph))
			val edgeless_graph = foldl (fn (node, graph) => addNode(graph, node)) empty new_nodes
		in
			foldl (fn (edge, graph) => addEdge(graph, edge)) edgeless_graph new_edges
		end
end