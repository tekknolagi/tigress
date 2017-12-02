open Common

exception InvalidMode of string

type mode = Parse | Type | Rename | Eval | Lower | Codegen
let mode_of_string s =
  let opts =
    ["parse",Parse; "type",Type; "rename", Rename; "eval",Eval; "lower",Lower;
    "codegen", Codegen]
  in
  match (L.assoc_opt s opts) with
  | Some opt -> opt
  | None -> raise @@ InvalidMode s

let string_of_mode = function
  | Parse -> "parse" | Type -> "type" | Rename -> "rename" | Eval -> "eval"
  | Lower -> "lower" | Codegen -> "codegen"

let const (type a) (type b) (x : a) (y : b) : a = x

exception BugInREPL of string

let _ =
  let default_mode = Lower in
  let mode =
    if Array.length Sys.argv < 2
    then default_mode
    else mode_of_string Sys.argv.(1)
  in
  print_endline @@ "REPL in " ^ string_of_mode mode ^ " mode.";
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      try
        if Unix.(isatty stdin) then ( print_string ">>> "; flush stdout );

        let to_print =
        let ast = L01parser.main L00lexer.token lexbuf in
        if mode=Parse
        then L02ast.string_of_aexp (const "") ast
        else
        let tst = L03typecheck.typecheck [] ast in
        if mode=Type
        then L02ast.string_of_aexp Types.string_of_ty tst
        else
        let ren = L04scoperesolution.rename [] tst in
        if mode=Rename
        then L02ast.string_of_aexp (const "") ren
        else
        let res = L05eval.eval [] ren in
        if mode=Eval
        then L02ast.string_of_value res
        else
        let low = L05mir.lower ren in
        if mode=Lower
        then String.concat "\n" @@ List.map L05mir.string_of_funrep low
        else raise @@ BugInREPL "Invalid mode"
        in
        print_endline to_print
      with
      | Failure _ -> exit 0
      | e -> ( print_endline @@ Printexc.to_string e;
               flush stdout )
    done
  with L00lexer.Eof ->
    exit 0
