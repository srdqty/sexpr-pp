signature LEXICAL_CONTEXT =
sig
  structure Mark : MARK
  structure Name : SYMBOL

  type t
  type rename = 
    { renamePhase : int
    , renameCtx : t
    , oldName : Name.t
    , newName : Name.t
    }
  type phase = int

  val resolve : t * Name.t * phase -> Name.t

  val empty : t
  val mark : t * Mark.t -> t
  val rename : t * rename -> t
  val applyRenames : t * rename list -> t
end
