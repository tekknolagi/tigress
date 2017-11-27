(* open L03mir *)

let const (type a) (type b) (x : a) (y : b) : a = x

let _ =
  try
    let basis = [] in
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        if Unix.(isatty stdin) then ( print_string ">>> "; flush stdout );
        let ast = L01parser.main L00lexer.token lexbuf in
        let tst = L02ast.typecheck basis ast in
        (*
        let ren = L02ast.rename [] tst in
        print_endline @@ L02ast.(string_of_aexp (const "") ren);
        *)

        let res = L03eval.eval basis tst in
        (
          print_endline @@ L02ast.string_of_value res;
          flush stdout
        )
        (*
        let (t, insts, funs) = L03mir.lower ren in
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
        *)

      with
      | Failure _ -> exit 0
      | e ->
      (
        print_endline @@ Printexc.to_string e;
        flush stdout
      )

    done
  with L00lexer.Eof ->
    exit 0
