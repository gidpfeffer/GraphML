(* Turns a string into a value *)
signature STRING_GENERATOR =
sig
	type gen_type
	val generate: string -> gen_type
end

signature GRAPHBUILDER = 
sig
	structure Graph : GRAPH
	type nodetype
	type edgetype
	(* String value for every node gets passed to generator which parses it to a value *)
	val build: string -> ((nodetype, edgetype) Graph.graph) option
end

structure StringKey = 
struct
	type ord_key = string
	val compare = String.compare
end

structure StringNode = Node(StringKey)
structure StringEdge = Edge(StringKey)(StringNode)
structure NMap = RedBlackMapFn(StringKey)
structure StringGraph = MapGraph(StringEdge)

(* Conforms to standard provided by the basicgraph.format file *)
(* NG turns node_val into a value, same for EG with edges *)
functor BasicGraphBuilder(NG: STRING_GENERATOR)(EG: STRING_GENERATOR) :> GRAPHBUILDER 
	where Graph = StringGraph 
	where type nodetype = NG.gen_type 
	where type edgetype = EG.gen_type
	=
struct
	type nodetype = NG.gen_type
	type edgetype = EG.gen_type
	structure Graph = StringGraph
	exception InvalidFile of string;

	fun fileToList filename =
		let 
			val file = TextIO.openIn filename
			val first_line = TextIO.inputLine file
		in
				case first_line of
				     SOME(text) => (case String.tokens Char.isSpace text of
				  		[num1, num2] => (valOf (Int.fromString num1), valOf (Int.fromString num2), file)
				  	  | _ => (raise InvalidFile "Need two numbers in first line"))
				   | NONE => (raise InvalidFile "Empty file provided")
			handle
				_ => (raise InvalidFile "Invalid file header, should be two numbers")
		end

	fun lineToNode line = case String.tokens Char.isSpace line of
		[node_key, node_val] => StringNode.gen (NG.generate node_val, node_key)
	  | _ => (raise InvalidFile "Invalid node line")

	fun lineToEdge (line, nodeMap) = 
		let
			fun findNode(node_key) = valOf (NMap.find (nodeMap, node_key))
		in
			case String.tokens Char.isSpace line of
			  [edge_key, sk, ek, "u", edge_val] => 
			  	StringEdge.genUndirected(findNode(sk), findNode(ek), EG.generate edge_val, edge_key)
			| [edge_key, sk, ek, "d", edge_val] => 
			  	StringEdge.genDirected(findNode(sk), findNode(ek), EG.generate edge_val, edge_key)
		  	| _ => (raise InvalidFile "Invalid node line")
		end

	fun pull_lines (0,file) = ([], file)
	  | pull_lines (num_lines,file) =
		  let
		  	val newLine = valOf (TextIO.inputLine file)
		  	val (lines, file) = pull_lines(num_lines - 1, file)
		  in
		  	(newLine::lines, file)
		  end

	fun buildWithException filename =
		let
			val (num_nodes, num_edges, file) = fileToList filename
			val (node_lines, file) = pull_lines(num_nodes,file)
			val (edge_lines, file) = pull_lines(num_edges,file)
			val nodes = map lineToNode node_lines
			val nodeMap = foldl (fn (node, map) => NMap.insert(map, StringNode.genKey node, node)) NMap.empty nodes
			val edges = map (fn line => lineToEdge(line, nodeMap)) edge_lines
			val graph_with_nodes = foldl (fn (node, graph) => Graph.addNode(graph, node)) Graph.empty nodes
			val graph_with_edges = foldl (fn (edge, graph) => Graph.addEdge(graph, edge)) graph_with_nodes edges
		in
			SOME(graph_with_edges)
		end

	(* TODO: add better error handling *)
	fun build filename = 
			buildWithException filename
		handle 
			Option => NONE
		  | _ => NONE
end