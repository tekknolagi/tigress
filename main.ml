let const (type a) (type b) (x : a) (y : b) : a = x

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        if Unix.(isatty stdin) then ( print_string ">>> "; flush stdout );
        let ast = L01parser.main L00lexer.token lexbuf in
        let tst = L03typecheck.typecheck [] ast in
        (* print_endline @@ L02ast.string_of_aexp Types.string_of_ty tst; *)
        let ren = L04scoperesolution.rename [] tst in
        (* let res = L05eval.eval [] ren in *)
        let funs = L05mir.lower ren in
        (
          print_endline @@ String.concat "\n" @@ List.map L05mir.string_of_funrep funs
        )
      with
      | Failure _ -> exit 0
      | e -> ( print_endline @@ Printexc.to_string e;
               flush stdout )
    done
  with L00lexer.Eof ->
    exit 0
