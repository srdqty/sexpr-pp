structure TextInputPort :> TEXT_INPUT_PORT =
struct
  type name = string
  type column = int
  type line = int
  type position = int
  type port = 
    TextIO.StreamIO.instream * name * column * line * position

  fun mkPort(instream, name) = (instream, name, 1, 1, 1)
  fun closePort(stream, _, _, _, _) = TextIO.StreamIO.closeIn stream
  fun getInstream(stream, _, _, _, _) = stream

  fun input1(stream, name, col, line, pos) =
    case TextIO.StreamIO.input1 stream of
      NONE => NONE
    | SOME(c, stream) =>
      if c = #"\n"
      then SOME(c, (stream, name, 1, line + 1, pos + 1))
      else SOME(c, (stream, name, col + 1, line, pos + 1))

  fun inputSpan((stream1, name, _, _, pos1), (_, _, _, _, pos2)) = let
    val (str, _) = TextIO.StreamIO.inputN(stream1, pos2 - pos1)
  in
    str
  end
  
  fun span((_, _, _, _, pos1), (_, _, _, _, pos2)) = pos2 - pos1

  fun name(_, nam, _, _, _) = nam
  fun position(_, _, _, _, pos) = pos
  fun column(_, _, col, _, _) = col
  fun line(_, _, _, lin, _) = lin
end
