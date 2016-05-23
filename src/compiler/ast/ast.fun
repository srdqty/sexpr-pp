structure Ast :> AST =
struct


datatype 'a t =
  Constant of 'a
| ShallowRef of int
| DeepRef of int * int
| GlobalRef of int
| If of 'a t * 'a t * 'a t
| Begin of 'a t list
(* Function with arity = n. *)
| FunFixArity of int * 'a t list
(* Function with arity >= n. *)
| FunVarArity of int * 'a t list
| Letrec of 'a t vector * 'a t list
(* 
* Arguments to let and app are stored in reverse
* order to avoid having to waste work reversing lists when
* operating on the arguments with tail recursive functions
*)
| Let of 'a t vector * 'a t list
| App of 'a t * 'a t vector


fun print(stream, printConstant, ast) = let
  fun output s = TextIO.output (stream, s)

  fun toList vec =
    Vector.foldr (fn (x, lst) => x :: lst) [] vec

  fun printList [] = ()
    | printList (hd :: []) = printAst hd
    | printList (hd :: tl) = 
        ( printAst hd
        ; output " "
        ; printList tl)


  and printVec vec = let
    val length = Vector.length vec
  in
    if 0 = length then (
    ) else (
      VectorSlice.app (fn x => (printAst x; output " "))
                      (VectorSlice.slice (vec, 0, SOME (length - 1)))
      ; printAst (Vector.sub (vec, length - 1))
    )
  end


  and printAst (Constant v) =
        ( output "(Constant "
        ; printConstant (stream, v)
        ; output ")")

    | printAst (ShallowRef i) =
        ( output "(ShallowRef "
        ; output (Int.toString i)
        ; output ")")

    | printAst (DeepRef (i, j)) =
        ( output "(DeepRef "
        ; output (Int.toString i)
        ; output " "
        ; output (Int.toString j)
        ; output ")")

    | printAst (GlobalRef i) =
        ( output "(GlobalRef "
        ; output (Int.toString i)
        ; output ")")

    | printAst (If (cond, thenAst, elseAst)) =
        ( output "(If "
        ; printAst cond
        ; output " "
        ; printAst thenAst
        ; output " "
        ; printAst elseAst
        ; output ")")

    | printAst (Begin body) =
        ( output "(Begin "
        ; printList body
        ; output ")")

    | printAst (FunFixArity (arity, body)) =
        ( output "(FunFixArity (Arity = "
        ; output (Int.toString arity)
        ; output ") "
        ; printList body
        ; output ")")

    | printAst (FunVarArity (minArity, body)) =
        ( output "(FunVarArity (Arity >= "
        ; output (Int.toString minArity)
        ; output ") "
        ; printList body
        ; output ")")

    | printAst (Let (rhss, body)) =
        ( output "(Let "
        ; output (Int.toString (Vector.length rhss))
        ; output " ("
        ; printVec (Vector.fromList (List.rev (toList rhss)))
        ; output ") "
        ; printList body
        ; output ")")

    | printAst (Letrec (rhss, body)) =
        ( output "(Letrec "
        ; output (Int.toString (Vector.length rhss))
        ; output " ("
        ; printVec rhss
        ; output ") "
        ; printList body
        ; output ")")

    | printAst (App (f, args)) =
        ( output "(App "
        ; printAst f
        ; output " (ArgCount "
        ; output (Int.toString (Vector.length args))
        ; output ") (Args "
        ; printVec (Vector.fromList (List.rev (toList args)))
        ; output "))")

in
  printAst ast
end


end
