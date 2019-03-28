signature UNION_FIND = 
sig
	structure Key : ORD_KEY
	type disjoint_sets
	val init : Key.ord_key list ->  disjoint_sets
	val find : disjoint_sets * Key.ord_key -> (disjoint_sets * Key.ord_key)
	val union : disjoint_sets * Key.ord_key *  Key.ord_key -> disjoint_sets
	val sets : disjoint_sets -> Key.ord_key list list
	val numSets : disjoint_sets -> int
end

functor MapUnionFind(K: ORD_KEY) :> UNION_FIND where Key = K =
struct
	structure Key = K
	structure Map = RedBlackMapFn(Key)
	exception UnknownKey

	type disjoint_sets = Key.ord_key Map.map

	fun init key_list = foldl (fn (k,m) => Map.insert(m, k, k)) Map.empty key_list

	fun find (m, key) = case Map.find(m, key) of
		NONE => raise UnknownKey
	  |	SOME(k) => (case Key.compare (key,k) of
			EQUAL => (m, k)
		  | _ => (
		  	let
		  		val (m, pointing_to)  = find(m, k)
		  	in
		  		(* Path compression *)
		  		(Map.insert (m, key, pointing_to), pointing_to)
		  	end
		  )
		)

	fun union (m, key1, key2) = case Map.find(m, key1) of 
		NONE => raise UnknownKey
	  | SOME(v1) => (
	  	case Map.find(m, key2) of
	  		NONE => raise UnknownKey
	  	  | SOME(v2) => Map.insert(m, v1, v2)
	  )

	fun sets m = 
		let
			fun mapKeyVal (k,v) =
				let
					val (uf,v) = find (m,k)
				in
					(k,v)
				end
			fun updateMap ((k,v), m) =
				case Map.find(m, v) of
					NONE => Map.insert(m, v, [k])
				  | SOME(l) => Map.insert(m, v, k::l)
			val res = foldl updateMap Map.empty (map mapKeyVal (Map.listItemsi m))
		in
			Map.listItems res
		end

	fun numSets m = length (sets m)
end