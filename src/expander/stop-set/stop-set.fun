functor StopSetFn (Symbol : SYMBOL) : STOP_SET =
struct


structure Symbol = Symbol

structure Key = 
struct
  type ord_key = Symbol.t
  fun compare (s, t) = Symbol.compare(s, t)
end

structure RedBlackSet = RedBlackSetFn(Key)

type t = RedBlackSet.set

val empty = RedBlackSet.empty

val member = RedBlackSet.member

val add = RedBlackSet.add

val addList = RedBlackSet.addList

val fromList = RedBlackSet.fromList

val union = RedBlackSet.union


end
