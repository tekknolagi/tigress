let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let basis = [] in
      let ast = L01parser.main L00lexer.token lexbuf in
      let tast = L02ast.typecheck basis ast in
      let str = L02ast.string_of_aexp Types.string_of_ty tast in
      print_endline str; flush stdout
    done
  with L00lexer.Eof ->
    exit 0
