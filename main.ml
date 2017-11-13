open L03mir

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        let basis = [] in
        let ast = L01parser.main L00lexer.token lexbuf in
        let tst = L02ast.typecheck basis ast in
        let ren = L02ast.rename [] tst in
        (
          print_endline @@ L02ast.string_of_aexp (fun _ -> "R") ren
        )

        (*
        let (t, insts, funs) = L03mir.lower tst in
        (
          let treeString = L03mir.string_of_tree t in
          let instructionsString =
            String.concat "\n" @@ List.map L03mir.string_of_inst insts
          in
          let funsString = "" in
          print_endline "----------";
          print_endline funsString;
          print_newline ();
          print_endline instructionsString;
          print_newline ();
          print_endline treeString;
          print_endline "----------";
          print_newline ();
        )
        *)

        (*
        let res = L03eval.eval basis tst in
        (
          print_endline @@ L02ast.string_of_value res;
          flush stdout
        )
        *)

      with Failure _ -> exit 0
      (* let str = L02ast.string_of_aexp Types.string_of_ty tast in
      print_endline str; flush stdout
      *)
    done
  with L00lexer.Eof ->
    exit 0
