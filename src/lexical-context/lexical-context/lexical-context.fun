functor LexicalContextFn
(
  structure Mark : MARK
  structure Name : SYMBOL
) : LEXICAL_CONTEXT =
struct


structure Mark = Mark
structure Name = Name

datatype t =
  EmptyCtx
| Mark of t * Mark.t
| Rename of t 
    * { renamePhase : int
      , renameCtx : t
      , oldName : Name.t
      , newName : Name.t
      }

type rename = 
  { renamePhase : int
  , renameCtx : t
  , oldName : Name.t
  , newName : Name.t
  }
type phase = int

val empty = EmptyCtx

fun mark (ctx, mark) = Mark(ctx, mark)

fun rename (ctx, renameData) = Rename(ctx, renameData)

fun applyRenames (ctx, []) = ctx
  | applyRenames (ctx, renameData :: rest) =
      applyRenames (rename (ctx, renameData), rest)

fun markCons (mark1, []) = [mark1]
  | markCons (mark1, mark2 :: rest) =
      if Mark.=(mark1, mark2) then (
        rest
      ) else (
        mark1 :: mark2 :: rest
      )

fun marksEq (m1 :: rest1, m2 :: rest2) =
      Mark.=(m1, m2) andalso marksEq (rest1, rest2)
  | marksEq (m1 :: rest1, []) = false
  | marksEq ([], m2 :: rest2) = false
  | marksEq ([], []) = true

fun marksof (EmptyCtx, stopName) = []
  | marksof (Mark (restCtx, mark), stopName) =
      markCons (mark, marksof (restCtx, stopName))
  | marksof ((Rename (restCtx, {newName, ...}), stopName)) = 
      if Name.=(newName, stopName) then [] else marksof (restCtx, stopName)

fun resolve (EmptyCtx, name, _) = name
  | resolve (Mark (restCtx, mark), name, phase) =
      resolve (restCtx, name, phase)
  | resolve (Rename (restCtx, {renamePhase, renameCtx, oldName, newName}),
             name,
             phase)
    =
      if phase = renamePhase then (
        let
          val name1 = resolve (renameCtx, oldName, 0)
          val name2 = resolve (restCtx, name, 0)
        in
          if Name.=(name1, name2) then (
            let
              val renameMarks = marksof (renameCtx, name1)
              val restMarks = marksof (restCtx, name2)
            in
              if marksEq (renameMarks, restMarks) then (
                newName
              ) else (
                resolve (restCtx, name, phase)
              )
            end
          ) else (
            resolve (restCtx, name, phase)
          )
        end
      ) else (
        resolve (restCtx, name, phase)
      )


end
