open Ast

type release_mode = 
  Never | Auto | Always | Best | AlwaysBest

type magical_flag = {
  mutable release_mode : release_mode;
  mutable warning_offs : string list;
  mutable warning_errs : string list;
  mutable mangle       : bool;
}

let magical_flag_default = {
  release_mode = Auto;
  warning_offs = [];
  warning_errs = [];
  mangle = true;
}

let get_end list = 
  List.hd (List.rev list) 

let get_magical_flags ast = 
  let mag = ref magical_flag_default in
  List.iter (function 
  | DCMagical(_, s) -> (
    let s = String.trim s in
    let sd = String.split_on_char ' ' s in
    match sd with
    | cmd :: arg :: _ -> (
      match cmd with
      | "warning_off" -> (
        mag := { !mag with warning_offs = arg :: !mag.warning_offs };
      )
      | "warning_err" -> (
        mag := { !mag with warning_errs = arg :: !mag.warning_errs };
      ) 
      | "release" -> (
        let mode = match arg with
        | "never" -> Never
        | "always" -> Always
        | "best" -> Best
        | "always_best" -> AlwaysBest
        | _ -> Auto
        in
        mag := { !mag with release_mode = mode };
      )
      | "mangle" -> (
        mag := { !mag with mangle = match arg with
        | "false" | "off" -> false
        | _ -> true };
      )
      | _ -> ()
    )
    | _ -> ()
  )
  | _ -> ()
  ) ast;
  !mag

let filter_warnings ls flags = 
  let simple = ref [] in 
  let errs = ref [] in
  let should_ignore = List.map ( fun s -> "[" ^ s ^ "]") flags.warning_offs in
  let should_err = List.map ( fun s -> "[" ^ s ^ "]") flags.warning_errs in
  List.iter (fun s -> (
    let s = String.trim s in 
    let signore = ref false in
    let serr = ref false in
    List.iter ( fun p -> (
      if String.starts_with ~prefix:p s then (
        signore := true
      )
    )) should_ignore;
    List.iter ( fun p -> (
      if String.starts_with ~prefix:p s then (
        serr := true
      )
    )) should_err;
    if not !signore then (
      if !serr then (
        errs := s :: !errs
      ) else (
        simple := s :: !simple
      )
    )
  )) ls;
  !simple, !errs