structure Error :> ERROR =
struct
  datatype t = 
    Error of
    { thunk: (unit -> string)
    , result : string option ref
    }

  fun fromString s =
    Error {thunk = (fn () => ""), result = ref (SOME s)}
  
  fun fromThunk thunk =
    Error {thunk = thunk, result = ref NONE}

  fun toString (Error {thunk, result = ref (SOME s) }) = s
    | toString (Error {thunk, result = result as (ref NONE)}) =
        let 
          val string = thunk ()
        in
          ( result := SOME string
          ; string
          )
        end
end
