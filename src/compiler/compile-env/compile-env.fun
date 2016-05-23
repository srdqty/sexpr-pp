functor CompileEnvFn (Symbol : SYMBOL) : COMPILE_ENV =
struct


structure Symbol = Symbol

type t = Symbol.t list list

datatype id_address =
  Free
| Shallow of int
| Deep of int * int

local
  fun shallow_lookup ([], _, _) = NONE
    | shallow_lookup (hd :: tl, sym, i) =
        if Symbol.=(sym, hd)
        then SOME i
        else shallow_lookup (tl, sym, i + 1)
  
  fun deep_lookup ([], _, _) = Free
    | deep_lookup (hd :: tl, sym, i) =
        (case shallow_lookup (hd, sym, 0) of
          NONE => deep_lookup (tl, sym, i + 1)
        | SOME j => Deep (i, j))
in
  fun lookup ([], _) = Free
    | lookup (hd :: tl, sym) =
        (case shallow_lookup (hd, sym, 0) of
          NONE => deep_lookup (tl, sym, 1)
        | SOME j => Shallow j)
end

fun extend (env, lst) = lst :: env

val empty = []


end
