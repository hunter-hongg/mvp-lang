open Ast

type anontyp = 
  Impl | Unsafe | Trusted | Usage | Param | Invalid

let typ_of_anon s = 
  let s = String.trim s in
  let sd = String.split_on_char ':' s in
  if List.length sd < 1 then 
    Invalid
  else 
    match List.hd sd with 
    | "impl" -> Impl
    | "unsafe" -> Unsafe
    | "trusted" -> Trusted
    | "usage" -> Usage
    | "param" -> Param
    | _ -> Invalid

let check_anon defs = 
  let pre = ref None in
  let cur = ref None in
  let warns = ref [] in 
  let warn loc msg = 
    warns := ("[W0003] " ^ (Util.format_loc loc) ^ ":" ^ msg) :: !warns in
  List.iter (fun d -> (
    pre := !cur;
    cur := Some d;
    match d with 
    | DFunc (loc, _, _, _, _)
    -> (
      match !pre with 
      | Some DCIntro (_, str) -> (
        match typ_of_anon str with 
        | Usage | Param 
        -> ()
        | _ -> warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
    | DFuncTrusted (loc, _, _, _, _)
    -> (
      match !pre with 
      | Some DCIntro (_, str) -> (
        match typ_of_anon str with 
        | Trusted | Usage | Param -> ()
        | _ -> warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
    | DFuncUnsafe (loc, _, _, _, _)
    | DCFuncUnsafe (loc, _, _, _, _)
    -> (
      match !pre with 
      | Some DCIntro (_, str) -> (
        match typ_of_anon str with 
        | Unsafe | Usage | Param -> ()
        | _ -> warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
    | DTest (loc, _, _)
    -> (
      match !pre with 
      | Some DCIntro (_, str) -> (
        match typ_of_anon str with 
        | Usage -> ()
        | _ -> warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
    | DStruct (loc, _, _)
    -> (
      match !pre with 
      | Some DCIntro (_, str) -> (
        match typ_of_anon str with 
        | Usage | Impl -> ()
        | _ -> warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
    | DCMagical (_, _)
    | DCIntro (_, _)
    -> ()
    | DModule (loc, _)
    | SImport (loc, _)
    | SImportAs (loc, _, _)
    | SImportHere (loc, _)
    | SExport (loc, _)
    -> (
      match !pre with 
      | Some DCIntro (_, _) -> (
        warn loc "invalid intro comment type"
      )
      | _ -> ()
    )
  )) defs;
  !warns