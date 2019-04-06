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
	(* Order is node, outbound, inbound *)
	type ('a,'b) nodeMap = (('a Node.node) * ((('a,'b) E.edge) EMap.map) * ((('a,'b) E.edge) EMap.map)) NMap.map
	datatype ('a,'b) graph = GRAPH of ('a,'b) nodeMap
	fun flatten xs = List.foldr (fn (x, acc) => x @ acc) [] xs
	val empty = GRAPH(NMap.empty)
	fun nodes (GRAPH(m)) = map (fn (node, outgoing, incoming) => node) (NMap.listItems m)
	
	fun edges (GRAPH(m)) = 
		let
			val allEdges = flatten (map (fn (node, outbound, inbound) => EMap.listItems outbound) (NMap.listItems m))
			val uniqueEdges = foldl (fn (edge, map) => EMap.insert(map, E.genKey edge, edge)) EMap.empty allEdges
		in
			map (fn (k,v) => v) (EMap.listItemsi uniqueEdges)
		end

	(* Ignores calls to add a node which is already present *)
	fun addNode ((GRAPH(m)), node) =
		case NMap.find (m, Node.genKey node) of
			SOME(_) => GRAPH(m)
		  | NONE => GRAPH(NMap.insert (m, Node.genKey node, (node, EMap.empty, EMap.empty)))
	(* Ignores calls to add a edge which is already present *)
	fun addEdge ((GRAPH(m)), edge) = 
		let
			val start_key = Node.genKey (E.genStart edge)
			val finish_key = Node.genKey (E.genFinish edge)
			(* Adds outbound edge to make genOutbound edges simple *)
			fun addOutgoingEdge (node_key, edge, (GRAPH(m))) = case NMap.find (m, node_key) of
				SOME((node, outgoing, incoming)) => (case (EMap.find (outgoing, E.genKey edge)) of
					  (* Dont add duplicates, just like nodes *)
					  SOME(edge) => GRAPH(m)
					  (* Edge wasnt present, so add it *)
					| NONE => GRAPH(NMap.insert (m, node_key, 
						(node, 
						EMap.insert (outgoing, E.genKey edge, edge),
						incoming))))
				(* Couldnt find node in graph *)
			  | NONE => raise NodeNotFound "Node from edge not present in graph"
			fun addIncomingEdge (node_key, edge, (GRAPH(m))) = case NMap.find (m, node_key) of
				SOME((node, outgoing, incoming)) => (case (EMap.find (incoming, E.genKey edge)) of
					  (* Dont add duplicates, just like nodes *)
					  SOME(edge) => GRAPH(m)
					  (* Edge wasnt present, so add it *)
					| NONE => GRAPH(NMap.insert (m, node_key, 
						(node,
						outgoing,
						EMap.insert (incoming, E.genKey edge, edge)))))
				(* Couldnt find node in graph *)
			  | NONE => raise NodeNotFound "Node from edge not present in graph"
			fun addEdges(start_key, end_key, edge, (GRAPH(m))) = 
				addIncomingEdge(end_key, edge, addOutgoingEdge(start_key, edge, GRAPH(m)))
		in
			addEdges(start_key, finish_key, edge, GRAPH(m))
		end

	fun selfEdge edge = case Edge.Node.Key.compare ((Edge.Node.genKey (Edge.genStart edge)),(Edge.Node.genKey (Edge.genFinish edge))) of
		EQUAL => true
	  | _ => false

	fun succ ((GRAPH(m)), node) = case NMap.find (m, Node.genKey node) of
		  SOME((node, outgoing, incoming)) => 
		  	foldl foldAddStartCheck (foldl foldAddFinish [] (EMap.listItems outgoing)) (EMap.listItems incoming)
		| NONE => raise NodeNotFound "Node to generate successors from not present in graph"
	
	and pred ((GRAPH(m)), node) = case NMap.find (m, Node.genKey node) of
	  SOME((node, outgoing, incoming)) => 
	  	foldl foldAddFinishCheck (foldl foldAddStart [] (EMap.listItems incoming)) (EMap.listItems outgoing)
	| NONE => raise NodeNotFound "Node to generate successors from not present in graph"
	
	and foldAddFinish (edge, l) = (Edge.genFinish edge, edge)::l
	
	and foldAddFinishCheck(edge, l) = case (E.genIsDirected edge orelse selfEdge edge) of
		true => l
	  | false => foldAddFinish(edge, l)

	and foldAddStart (edge, l) = (Edge.genStart edge, edge)::l
	
	and foldAddStartCheck(edge, l) = case (E.genIsDirected edge orelse selfEdge edge) of
		true => l
	  | false => foldAddStart(edge, l)

	fun nodeExists ((GRAPH(m)), node) = case NMap.find (m, Node.genKey node) of
		SOME(_) =>  true
	  | NONE => false

	fun edgeExists ((GRAPH(m)), edge) = case NMap.find (m, Node.genKey (E.genStart edge)) of
		SOME((node, edge_map, _)) => (case (EMap.find (edge_map, E.genKey edge)) of
			  (* Dont add duplicates, just like nodes *)
			  SOME(edge) => true
			  (* Edge wasnt present, so add it *)
			| NONE => false)
		(* Couldnt find node in graph *)
	  | NONE => false


	fun findNode ((GRAPH(m)),key) = case NMap.find(m, key) of
		NONE => NONE
	  |	SOME((node, _, _)) => SOME(node)

	fun removeEdge ((GRAPH(m)),edge) = 
		let
			val edgeKey = Edge.genKey edge
			val startNode = Edge.genStart edge
			val finishNode = Edge.genFinish edge
			fun removeOutboundEdgeFromNode node edge m = case NMap.find(m, Edge.Node.genKey node) of
				NONE => m
			  | SOME((node, outbound, inbound)) => (
			  	let
			  		val (newEdgeMap, edge) = EMap.remove(outbound, Edge.genKey edge)
			  	in
			  		NMap.insert(m, Edge.Node.genKey node, (node, newEdgeMap, inbound))
			  	end
			  )
			fun removeInboundEdgeFromNode node edge m = case NMap.find(m, Edge.Node.genKey node) of
				NONE => m
			  | SOME((node, outbound, inbound)) => (
			  	let
			  		val (newEdgeMap, edge) = EMap.remove(inbound, Edge.genKey edge)
			  	in
			  		NMap.insert(m, Edge.Node.genKey node, (node, outbound, newEdgeMap))
			  	end
			  )
			fun removeEdges startNode endNode edge m = 
				(removeInboundEdgeFromNode endNode edge (removeOutboundEdgeFromNode startNode edge m))
		in
			GRAPH(removeEdges startNode finishNode edge m)
		end

	fun removeNode ((GRAPH(m)),node) = 
		let
			fun outboundEdges (node, m) = case NMap.find(m, Edge.Node.genKey node) of
				NONE => []
			  | SOME((node, outgoing, _)) => EMap.listItems(outgoing)
			fun inboundEdges (node, m) = case NMap.find(m, Edge.Node.genKey node) of
				NONE => []
			  | SOME((node, _, incoming)) => EMap.listItems(incoming)
			val outbound = outboundEdges (node, m)
			val inbound = inboundEdges (node, m)
			val GRAPH(m) = foldl (fn (edge, graph) => removeEdge (graph, edge)) (GRAPH(m)) outbound
			fun removeIfNotSelf (edge, (graph, removed)) = if (selfEdge edge) then (graph, removed) else ((removeEdge (graph, edge)), edge::removed)
			val (GRAPH(m), removed) = foldl removeIfNotSelf (GRAPH(m), outbound) inbound
			val (mapWithoutNode, removedNode) = NMap.remove(m, Edge.Node.genKey node)
		in
			(GRAPH(mapWithoutNode), removed)
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