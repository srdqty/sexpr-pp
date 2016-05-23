functor MarkFn (Ignore : sig end) :> MARK =
struct
  type t = int

  val markRef = ref 0

  val null = ~1

  fun freshMark() = let
    val nextMark = !markRef
    in
      markRef := !markRef + 1;
      nextMark
    end

  fun eq(m1, m2) = m1 = m2
  fun op =(m1,m2) = eq(m1,m2)

  fun lt(m1, m2) = m1 < m2
  val op < = lt

  fun toInt m = m

  fun toString m = Int.toString m
end
