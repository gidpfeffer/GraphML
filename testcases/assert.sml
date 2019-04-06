signature ASSERT = 
sig
	val assertEqual: ''a * ''a * (unit -> unit) -> unit
end

structure Assert = 
struct
	fun assertEqual (v1,v2,f) = if v1 = v2 then () else f()
end