functor SymbolTableFn (Ignore : sig end) :> SYMBOL_TABLE =
struct


(* 
* The symbol table is implemented with a hash table
* that maps strings to integers. And a dynamic array
* that mapps contiguous integers to strings.
* This enables symbols to be compared with integer comparison,
* avoiding a more costly string data comparison.
* Hashing symbols also because equivalent in cost to hashing
* integers.
*)

(* 
* Hash key structure used as input to the hash table
* functor.
*)
structure StringHashKey =
struct
  type hash_key = string
  fun hashVal s = MLton.hash s
  fun sameKey (s, t) = s = t
end
exception NotFound
structure HT = HashTableFn (StringHashKey)

type t = (int HT.hash_table * string DynamicArray.array)

val stringToIntTable = HT.mkTable (256, NotFound)
val intToStringTable = DynamicArray.array (256, "")

val table = (stringToIntTable, intToStringTable)

structure Symbol =
struct
  type t = int
  fun eq (a, b) = a = b
  fun op =(a, b) = eq (a, b)

  val hash = MLton.hash

  val compare = Int.compare

  fun toString i = DynamicArray.sub (intToStringTable, i)

  fun fromString str =
    case HT.find stringToIntTable str of
      NONE => 
        let 
          val nextInt = 1 + (DynamicArray.bound intToStringTable)
        in
          HT.insert stringToIntTable (str, nextInt)
          ; DynamicArray.update (intToStringTable, nextInt, str)
          ; nextInt
        end
    | SOME i => i

  val empty = fromString ""

  fun freshSym str =
  let
    fun loop i =
      let 
        val sym = String.^ (str, Int.toString i)
      in
        case HT.find stringToIntTable sym of
          NONE => 
            let 
              val nextInt = 1 + (DynamicArray.bound intToStringTable)
            in
              HT.insert stringToIntTable (sym, nextInt)
              ; DynamicArray.update (intToStringTable, nextInt, sym)
              ; nextInt
            end
        | SOME _ => loop (i + 1)
      end
  in
    loop 0
  end
end

fun app f = HT.app f stringToIntTable


end
