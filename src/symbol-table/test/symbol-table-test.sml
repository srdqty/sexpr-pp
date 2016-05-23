structure Tbl1 = SymbolTableFn(struct end)
structure Tbl2 = SymbolTableFn(struct end)

val str1 = "abc"
val str2 = "def"
val str3 = "ghi"
val str4 = "jkl"
val str5 = "mnl"

val sym11 = Tbl1.Symbol.fromString str1
val sym12 = Tbl1.Symbol.fromString str2
val sym13 = Tbl1.Symbol.fromString str3
val sym14 = Tbl1.Symbol.fromString str4
val sym15 = Tbl1.Symbol.fromString str5

val sym21 = Tbl2.Symbol.fromString str1
val sym22 = Tbl2.Symbol.fromString str2
val sym23 = Tbl2.Symbol.fromString str3
val sym24 = Tbl2.Symbol.fromString str4
val sym25 = Tbl2.Symbol.fromString str5

val str11 = List.hd (CommandLine.arguments())
val () = print ( "str1 = str11? "
               ^ (Bool.toString (MLton.eq (str1, str11)))
               ^ "\n")
val sym111 = Tbl1.Symbol.fromString str11

val cmp = Tbl1.Symbol.= (sym11, sym111)
val () = print  ( "sym11 = sym111? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym11, sym12)
val () = print  ( "sym11 = sym12? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym11, sym13)
val () = print  ( "sym11 = sym13? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym11, sym14)
val () = print  ( "sym11 = sym14? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym11, sym15)
val () = print  ( "sym11 = sym15? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )


                (*
val cmp = Tbl1.Symbol.= (sym21, sym21)
val () = print  ( "sym21 = sym21? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym21, sym22)
val () = print  ( "sym21 = sym22? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym21, sym23)
val () = print  ( "sym21 = sym23? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym21, sym24)
val () = print  ( "sym21 = sym24? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
val cmp = Tbl1.Symbol.= (sym21, sym25)
val () = print  ( "sym21 = sym25? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
                *)

                (*
val cmp = Tbl1.Symbol.= (sym11, sym21)
val () = print  ( "sym11 = sym21? "
                ^ (Bool.toString cmp)
                ^ "\n"
                )
                *)

val () =
  Tbl1.app (fn x => print ((Tbl1.Symbol.toString x) ^ "\n"))
