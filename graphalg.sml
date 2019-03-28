functor GraphAlg(G: GRAPH) :> GRAPHALG where Graph = G =
struct
	structure Graph = G
	structure NMap = RedBlackMapFn(Graph.Edge.Node.Key)
	structure UF = MapUnionFind(Graph.Edge.Node.Key)
	exception ImplementationError of string
	exception Impossible

	fun bellmanFord (graph, start, edge_fn) =
		let
			val table = ref (foldl (fn (node, table) => NMap.insert(table, Graph.Edge.Node.genKey node, NONE)) NMap.empty (Graph.nodes graph))
			val _ = table := NMap.insert(!table, Graph.Edge.Node.genKey start, SOME((0.0, NONE)))
			val n = ref (length (Graph.nodes graph))
			fun updateEdge (edge) = 
				let
					val start = Graph.Edge.genStart edge
					val finish = Graph.Edge.genFinish edge
					fun updateNode (startNode, finishNode, weight, edge) =
						let
							val start = Graph.Edge.Node.genKey startNode
							val finish = Graph.Edge.Node.genKey finishNode
							val startDistance = valOf (NMap.find(!table, start))
							val finishDistance = valOf (NMap.find(!table, finish))
						in
							case startDistance of 
								NONE => ()
							  | SOME((ds,_)) => (
							  	case finishDistance of
							  		NONE => table := NMap.insert(!table, finish, SOME((ds + weight, SOME((edge, startNode, finishNode)))))
							  	  | SOME((df,_)) => 
							  	  	if df > (ds + weight)
							  	  	then table := NMap.insert(!table, finish, SOME((ds + weight, SOME((edge, startNode, finishNode))))) 
							  	  	else ()
							  )
						end
					val _ = updateNode(start, finish, edge_fn edge, edge)
				in
					if Graph.Edge.genIsDirected edge 
					then ()
					else updateNode(finish, start, edge_fn edge, edge)
				end
			val _ = while (!n > 0) do (n := !n - 1; map (fn edge => updateEdge(edge)) (Graph.edges graph); ())
			val nTable = !table
			val _ = map (fn edge => updateEdge(edge)) (Graph.edges graph)
		in
			(nTable, !table)
		end

	fun negativeCycle (nTable, nPlusOneTable) =
		let
			val eps = 0.000001
			fun ensureEquality (key, map1, map2) =
				case NMap.find(map1, key) of
					SOME(val1) => (case NMap.find(map2, key) of
						SOME(val2) => (
							case val1 of
								SOME((v1,_)) => (case val2 of
									SOME((v2,_)) => Real.abs (v1-v2) < eps
								  |	NONE => false)
							  |	NONE => (case val2 of
									SOME((v2,_)) => false
								  |	NONE => true))
					  | NONE => false)
				  | NONE => (case NMap.find(map2, key) of
						SOME(val2) => false
					  | NONE => true)
			val equalLength = (length (NMap.listItems nTable)) = (length (NMap.listItems nPlusOneTable))
			val equality = foldl (fn ((k,v), eq) => eq andalso ensureEquality(k, nTable, nPlusOneTable)) true (NMap.listItemsi nTable)
		in
			not (equalLength andalso equality)
		end

	fun negativeCycleExists (graph, start) edge_fn = negativeCycle (bellmanFord(graph, start, edge_fn))

	fun shortestPathLength (graph, start, finish) edge_fn = 
		let
			val (table1, table2) = bellmanFord(graph, start, edge_fn)
		in
			if negativeCycle (table1, table2) 
			then NONE
			else case valOf (NMap.find(table1, Graph.Edge.Node.genKey finish)) of
				NONE => NONE
			  | SOME(w,_) => SOME(w)
		end

	fun shortestPath (graph, start, finish) edge_fn = 
		let
			val (table1, table2) = bellmanFord(graph, start, edge_fn)
		in
			if negativeCycle (table1, table2) 
			then NONE
			else case valOf (NMap.find(table1, Graph.Edge.Node.genKey finish)) of
				NONE => NONE
			  | SOME(w,edgeOp) => (
			  	let
			  		fun getPath(curr, currPath) =
			  			case valOf (NMap.find(table1, Graph.Edge.Node.genKey curr)) of
			  				NONE => raise Impossible
			  			  | SOME((w,edgeOp)) => 
			  			  		case edgeOp of
			  			  			SOME(e,s,f) => getPath(s, e::currPath)
			  			  		  | NONE => currPath
			  	in
			  		SOME(getPath(finish, []))
			  	end
			  )
		end

	fun kruskals graph (edge_fn: ('a,'b) Graph.Edge.edge -> real) = 
		let
			val sorted_edges = ListMergeSort.sort (fn (edge1, edge2) => (edge_fn edge1) > (edge_fn edge2)) (Graph.edges graph)
			val uf = UF.init (map (fn node => Graph.Edge.Node.genKey node) (Graph.nodes graph))
			fun processEdge(edge, (graph, uf)) =
				let
					val start = Graph.Edge.genStart edge
					val finish = Graph.Edge.genFinish edge
					val (uf, findStart) = UF.find (uf, (Graph.Edge.Node.genKey start))
					val (uf, findFinish) = UF.find (uf, (Graph.Edge.Node.genKey finish))
				in
					case Graph.Edge.Node.Key.compare (findStart, findFinish) of
						EQUAL => (graph, uf)
					  | _ => (
					  	let
					  		val graph = 
						  		if (Graph.nodeExists (graph, start))
						  		then graph
						  		else Graph.addNode (graph, start)
						  	val graph = 
						  		if (Graph.nodeExists (graph, finish))
						  		then graph
						  		else Graph.addNode (graph, finish)
					  	in
					  		(Graph.addEdge (graph, edge), UF.union (uf, (Graph.Edge.Node.genKey start), (Graph.Edge.Node.genKey finish)))
					  	end
					  )
				end
			val (newGraph, newUF) = foldl processEdge (Graph.empty, uf) sorted_edges
		in
			if UF.numSets newUF = 1 then SOME(newGraph) else NONE
		end

	fun mst graph edge_fn = kruskals graph edge_fn

	fun transpose graph = 
		let
			val nodes = Graph.nodes(graph)
			val old_edges = Graph.edges(graph)
			fun edge_map_fn edge = case Graph.Edge.genIsDirected edge of 
				true => (
					let
						val start = Graph.Edge.genStart edge
						val finish = Graph.Edge.genFinish edge
						val value = Graph.Edge.genVal edge
						val key = Graph.Edge.genKey edge
					in
						Graph.Edge.genDirected(finish, start, value, key)
					end
				)
			  | false => edge
			val new_edges = map edge_map_fn old_edges
			val edgeless_graph = foldl (fn (node, graph) => Graph.addNode(graph, node)) Graph.empty nodes
		in
			foldl (fn (edge, graph) => Graph.addEdge(graph, edge)) Graph.empty new_edges
		end

end


