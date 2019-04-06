signature NODE = 
sig
	structure Key : ORD_KEY
	type 'a node
	val gen : 'a * Key.ord_key -> 'a node
	val genVal : 'a node -> 'a
	val genKey : 'a node -> Key.ord_key
end

signature EDGE = 
sig
	structure Key : ORD_KEY
	structure Node : NODE
	type ('a,'b) edge
	val genVal : ('a,'b) edge -> 'b
	val genKey : ('a,'b) edge -> Key.ord_key
	val genStart : ('a,'b) edge -> 'a Node.node
	val genFinish : ('a,'b) edge -> 'a Node.node
	val genIsDirected : ('a,'b) edge -> bool
	val genDirected : 'a Node.node * 'a Node.node * 'b * Key.ord_key -> ('a,'b) edge
	val genUndirected : 'a Node.node * 'a Node.node * 'b * Key.ord_key -> ('a,'b) edge
end

signature GRAPH = 
sig
	structure Edge : EDGE
	type ('a,'b) graph
	val empty : ('a,'b) graph
	val nodes : ('a,'b) graph -> 'a Edge.Node.node list
	val edges : ('a,'b) graph -> ('a,'b) Edge.edge list
	val addNode : ('a,'b) graph * 'a Edge.Node.node -> ('a,'b) graph
	val addEdge : ('a,'b) graph * ('a,'b) Edge.edge -> ('a,'b) graph
	val removeEdge : ('a,'b) graph * ('a,'b) Edge.edge -> ('a,'b) graph
	(* New graph with node removed and a list of all adjacent edges that were removed *)
	val removeNode : ('a,'b) graph * 'a Edge.Node.node -> ('a,'b) graph * ('a,'b) Edge.edge list
	val nodeExists : ('a,'b) graph * 'a Edge.Node.node -> bool
	val edgeExists : ('a,'b) graph * ('a,'b) Edge.edge -> bool
	val findNode : ('a,'b) graph * Edge.Node.Key.ord_key -> ('a Edge.Node.node) option
	val pred: ('a,'b) graph * 'a Edge.Node.node -> ('a Edge.Node.node * ('a,'b) Edge.edge) list
	val succ: ('a,'b) graph * 'a Edge.Node.node -> ('a Edge.Node.node * ('a,'b) Edge.edge) list
	(* Changes the values of the nodes and edges *)
	val transformGraph: (('a Edge.Node.node -> 'c Edge.Node.node) * (('a,'b) Edge.edge -> ('c,'d) Edge.edge)) -> ('a,'b) graph -> ('c,'d) graph
end