signature STACK =
sig
  type 'a stack
  exception EmptyStack

  val empty : 'a stack

  val push : 'a stack * 'a -> 'a stack
  val pop : 'a stack -> 'a * 'a stack
  val top : 'a stack -> 'a
  val drop : 'a stack -> 'a stack
end
