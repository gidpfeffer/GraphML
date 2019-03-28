signature GRAPHALG = 
sig
	structure Graph : GRAPH
	(* Return dfs trees *)

	(* Return dfs trees *)
(*	val dfsFrom : ('a,'b) Graph.graph * 'a Node.node -> ('a,'b) Graph.graph*)

	(* Return bfs trees *)
(*	val bfsFrom : ('a,'b) Graph.graph * 'a Node.node -> ('a,'b) Graph.graph*)

	(* Return all connected components as seperate graphs *)
(*	val genConnectedComponents : ('a,'b) Graph.graph -> ('a,'b) Graph.graph list*)

	(* Run astar with curried arguments:
		graph to run astar from
		function which evaluates heuristic at every node
		function which derives edge weight from every edge
		predicate indicating goal state arrival (goal function)
	 *)
(*	val astar : ('a,'b) Graph.graph -> 
		('a Graph.Node.node -> real) -> 
		(('a, 'b) Graph.Edge.edge -> real) -> 
		('a Graph.Node.node -> bool) ->
		(('a, 'b) Graph.Edge.edge list) option*)

	val shortestPathLength : 
		('a,'b) Graph.graph * 'a Graph.Edge.Node.node * 'a Graph.Edge.Node.node -> 
		(('a, 'b) Graph.Edge.edge -> real) -> 
		real option

	val negativeCycleExists : 
		('a,'b) Graph.graph * 'a Graph.Edge.Node.node -> 
		(('a, 'b) Graph.Edge.edge -> real) -> 
		bool

	val shortestPath : 
		('a,'b) Graph.graph * 'a Graph.Edge.Node.node * 'a Graph.Edge.Node.node -> 
		(* Weight function *)
		(('a, 'b) Graph.Edge.edge -> real) -> 
		(* Path *)
		('a, 'b) Graph.Edge.edge list option

	(* Return minimum spannning tree if input graph was connected with curried arguments:
		graph to run mst alg from
		function which derives edge weight from every edge
	 *)
	val mst : 
		('a,'b) Graph.graph -> 
		(('a, 'b) Graph.Edge.edge -> real) -> 
		('a, 'b) Graph.graph option

	val transpose :
		('a,'b) Graph.graph -> ('a, 'b) Graph.graph

end