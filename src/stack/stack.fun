structure Stack :> STACK =
struct
  type 'a stack = 'a list
  exception EmptyStack

  val empty = []

  fun push(stack, v) = v :: stack

  fun pop(top :: stack) = (top, stack)
    | pop [] = raise EmptyStack

  fun top(top :: stack) = top
    | top [] = raise EmptyStack

  fun drop(_ :: stack) = stack
    | drop [] = raise EmptyStack
end
