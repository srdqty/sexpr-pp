signature TEXT_INPUT_PORT =
sig
  type port

  val mkPort : TextIO.StreamIO.instream * string -> port
  val closePort : port -> unit
  val getInstream : port -> TextIO.StreamIO.instream

  val input1 : port -> (char * port) option
  (* Read or count characters between port points,
     includes the character read by the first port
     and does not include the character (if any) read
     by the last port *)
  val inputSpan : port * port -> string
  val span : port * port -> int

  val name : port -> string
  val position : port -> int
  val column : port -> int
  val line : port -> int
end
