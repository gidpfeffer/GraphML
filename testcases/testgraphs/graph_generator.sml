
signature GRAPHGEN = 
sig
	type nodetype
	type edge_type
	structure TestGraph : GRAPH
	val genGraph : (nodetype, edge_type) TestGraph.graph
end