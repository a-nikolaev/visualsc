
open Printf
open Common

(* a Getopt *)

type sym = Short of char | Long of string

type 'a result = OK of 'a | Fail

module Sym = struct type t = sym let compare = compare end
module S = Set.Make(Sym)
module M = Map.Make(Sym)

(* split long and short options *)
let split ls =  
  List.fold_left 
    (fun (shorts, longs) s -> match s with (Short c) -> (String.make 1 c)::shorts, longs | Long s -> shorts, s::longs) 
    ([], []) ls

(* list of chars *)
let list_of_string s = fold_lim (fun acc i -> s.[i]::acc) [] 0 (String.length s - 1) 

let flagset_of_string s =
  s
  |> list_of_string
  |> List.map (fun c -> Short c)
  |> S.of_list 


let scan flags_ls params_ls arr index_start =
  let short_flags, long_flags = split flags_ls in
  let short_params, long_params = split params_ls in
  
  let str_short_flags = String.concat "" short_flags in
  let str_short_params = String.concat "" short_params in

  (* Regular expressions for 5 cases: *)
  (* I     (1..)    
          -abcde   *)
  let rx_short_flags = Str.regexp (Printf.sprintf "^-\\([%s]+\\)$" str_short_flags) in
  (* II    (1..)2(3..)  
          -abcdeXparam   
          -abcdeX ...    
               -X ...    *)
  let rx_short_flags_param = Str.regexp (Printf.sprintf "^-\\([%s]*\\)\\([%s]\\)\\(.*\\)$" str_short_flags str_short_params) in
  (* III    (1.)  
          --name         *)
  let rx_long_flag = Str.regexp (Printf.sprintf "^--\\(%s\\)$" (String.concat "\\|" long_flags)) in
  (* IV     (1.) 
          --name ...     
          --name= ...     *)
  let rx_long_param1 = Str.regexp (Printf.sprintf "^--\\(%s\\)=?$" (String.concat "\\|" long_params)) in
  (* V      (1.) (2..) 
          --name=param   *)
  let rx_long_param2 = Str.regexp (Printf.sprintf "^--\\(%s\\)=\\(.+\\)$" (String.concat "\\|" long_params)) in


  (* helpers *)
  let match_rx rx s = Str.string_match rx s 0 in 
  let flagset_add str set = (flagset_of_string str) |> S.union set in

  let len = Array.length arr in

  (* main iteration *)
  let rec next i (flagset, parammap, other_args) = 
    if i >= len then (flagset, parammap, List.rev other_args)
    else
    ( 
      let s = arr.(i) in
      let m i = Str.matched_group i s in
      
      if s = "--" then                             (* stop *)
        let args = List.rev_append other_args ((Array.sub arr (i+1) (len-i-1)) |> Array.to_list) in
        (flagset, parammap, args)      

      else if match_rx rx_short_flags s then       (* I  *)
        next (i+1) (flagset_add (m 1) flagset, parammap, other_args)

      else if match_rx rx_short_flags_param s then (* II *)
        let new_flagset = flagset_add (m 1) flagset in
        let sym = Short (m 2).[0] in
        ( match m 3 with 
          | "" when i+1 < len -> next (i+2) (new_flagset, M.add sym (OK arr.(i+1)) parammap, other_args)
          | "" -> (new_flagset, M.add sym Fail parammap, other_args)
          | param -> next (i+1) (new_flagset, M.add sym (OK param) parammap, other_args)
        )
      else if match_rx rx_long_flag s then         (* III *)
        next (i+1) (S.add (Long (m 1)) flagset, parammap, other_args)

      else if match_rx rx_long_param1 s then       (* IV  *)
        ( if i+1 < len then next (i+2) (flagset, M.add (Long (m 1)) (OK arr.(i+1)) parammap, other_args)
          else (flagset, M.add (Long (m 1)) Fail parammap, other_args)
        )
      else if match_rx rx_long_param2 s then       (* V   *)
        next (i+1) (flagset, M.add (Long (m 1)) (OK (m 2)) parammap, other_args)

      else                                         (* otherwise *)
        next (i+1) (flagset, parammap, s::other_args)
    )
  in

  (* collect all data *)
  let flagset, parammap, other_args = next index_start (S.empty, M.empty, []) in

  (* print out *)
  (*
  S.iter (function Short c -> printf "\'%c\'\n" c | Long s -> printf "\"%s\"\n" s) flagset;
  printf "\n";
  M.iter (fun k v ->
    let sv = match v with OK s -> sprintf "\"%s\"" s | Fail -> "Fail" in
    match k with Short c -> printf "\'%c\' = %s\n" c sv | Long s -> printf "\"%s\" = %s\n" s sv
  ) parammap;
  printf "\n";
  List.iter (fun s -> printf "%s\n" s) other_args;
  *)

  (* return functions *)
  let get_flag sym = S.mem sym flagset in
  let get_param sym = 
    try Some (M.find sym parammap) with Not_found -> None
  in
 
  (get_flag, get_param, other_args)
