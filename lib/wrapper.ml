open Printf

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let get_head lst = 
  match lst with
  | [] -> None
  | h :: _ -> Some h

let cxx_deal_module name = 
  let nmod = if String.starts_with ~prefix:"std" name then 
    "mvp_std." ^ (String.concat "." ((String.split_on_char '.' name) |> List.tl))
  else name in
  nmod

let parse_input_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Parsing.Parse_error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      eprintf "Syntax error at %s:%d:%d\n%!" filename line col;
      exit 1
  | Lexer.SyntaxError msg ->
      eprintf "Lexer error: %s\n%!" msg;
      exit 1
  | _ ->
      eprintf "Parsing error\n%!";
      exit 1

let toml_get_mod import = 
  if String.starts_with ~prefix:"c:" import then 
      None
  else (
      let toml = read_file "miva.toml" in
      match Toml.Parser.from_string toml with 
      | `Ok table -> (
      try 
          match Toml.Types.Table.find (Toml.Min.key "project") table with 
          | Toml.Types.TTable t -> (
          match Toml.Types.Table.find (Toml.Min.key "name") t with 
          | Toml.Types.TString s -> (
              let raw_module = ( if String.starts_with ~prefix:s import then
                  ("src/" ^ (String.concat "/" ((String.split_on_char '/' import) |> List.tl))) ^ ".miva"
              else 
                  let pstk = String.split_on_char '/' import in
                  (try Sys.getenv "MIVA_STD" with _ -> ".") ^ "/" ^ (
                    match get_head pstk with 
                    | Some h -> h
                    | None -> ""
                  ) 
                  ^ "/src/" ^ (String.concat "/" (List.tl pstk)) ^ ".miva"
              ) in
              let raw_module_name = (parse_input_file raw_module) in
              let raw_module_mod = (Symbol_table.build_symbol_table raw_module_name).module_name in
              Some raw_module_mod
          )
          | _ -> None
          )
          | _ -> None
      with 
          | _ -> None
      )
      | _ -> None
  )

let deal_std name = 
  if String.starts_with ~prefix:"std" name then 
    "mvp_std." ^ (String.concat "." ((String.split_on_char '.' name) |> List.tl))
  else name

let toml_get_mod_std import = 
  match toml_get_mod import with 
  | Some i -> (
    Some (deal_std i)
  )
  | None -> None

let toml_deal_ias import alias = 
  match toml_get_mod import with 
  | Some i -> (
    let ai = cxx_deal_module alias in 
    let di = cxx_deal_module i in
    Some [di; ai]
  )
  | None -> None
