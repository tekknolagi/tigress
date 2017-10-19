let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let ast = L01parser.main L00lexer.token lexbuf in
      let str = L02ast.string_of_exp ast in
      print_endline str; flush stdout
    done
  with L00lexer.Eof ->
    exit 0
