open Printf
open Scanf

open Common

(* facet *)
module S = Set.Make(struct type t = int let compare = compare end) 

(* subset *)
let ( << ) = S.subset
let ( >> ) f1 f2 = f2 << f1

(* simplicial complex = set of facets *)
module SS = Set.Make(struct type t = S.t let compare = S.compare end)

(* read from list *)
let s_of_list = S.of_list 

let ss_of_list ls = ls |> List.map s_of_list |> SS.of_list 

(* External Interface *)

let face_is_included fc sc = 
  SS.exists (fun ft -> fc << ft) sc

let add_face fc sc =
  if face_is_included fc sc then sc else
    sc 
    |> SS.filter (fun ft -> not (ft << fc))
    |> SS.add fc

(* flatten the sc *)
let nodes_set sc =
  let ls = SS.elements sc in
  match ls with
  | [] -> S.empty
  | hd::[] -> hd
  | hd::tl -> List.fold_left (fun acc f -> S.union f acc) hd tl

(* number of nodes *)
let number_of_nodes sc = sc |> nodes_set |> S.cardinal

(* find all facets the nodes belongs to *)
let node_belongs_to sc e = SS.filter (fun f -> S.mem e f) sc

(* merge two SCs removing all subsets *)
let merge sc1 sc2 = 
  SS.fold (fun f sc_acc -> add_face f sc_acc) sc2 sc1

let (<<<) sc1 sc2 = 
  SS.for_all (fun facet -> SS.exists (fun bigger_facet -> facet << bigger_facet ) sc2) sc1

let (>>>) sc1 sc2 = sc2 <<< sc1

(* make a SC of subfacets of ft, which don't contain exactly one element of rmft *)
let fan_remove_each_of rmft ft =
  S.fold (fun e sc_acc -> SS.add (S.remove e ft) sc_acc) rmft SS.empty 

(* remove with all smaller faces that are not in other facets *)
let remove_facet_simple ft sc =
  SS.filter (fun f -> not (S.equal f ft)) sc

(* real facet removal *)
let remove_facet ft sc = 
  if SS.exists (S.equal ft) sc then
    (* remove the facet from the set of facets *)
    let sc1 = remove_facet_simple ft sc in
    (* add all smaller faces *)
    let sc_addon = fan_remove_each_of ft ft in
    merge sc1 sc_addon
  else
    sc

let remove_face fc sc =
  if face_is_included fc sc then
    (* sc1 = SC with all the facets that did not contain the face fc *)
    let sc1, removed_ss = SS.partition (fun f -> not (fc << f)) sc in
    SS.fold 
      ( fun removed_ft sc_acc -> 
          let sc_addon = fan_remove_each_of fc removed_ft in
          merge sc_acc sc_addon
      ) removed_ss sc1
  else
    sc

let facet_degree sc e = 
  SS.fold (fun ft count -> if S.mem e ft then count+1 else count) sc 0

(* printers *)
let s_fprint ch s = 
  fprintf ch "{ ";
  S.iter (fun i -> fprintf ch "%i " i) s;
  fprintf ch "} "

let s_print = s_fprint stdout

let ss_fprint ch ss =
  SS.iter (fun s -> s_fprint ch s; printf "\n") ss

let ss_fprint_straight ch ss =
  print_string "[ ";
  SS.iter (fun s -> s_fprint ch s; printf " ") ss;
  print_string "]\n"

let ss_print = ss_fprint stdout
let ss_print_straight = ss_fprint_straight stdout

(* parsers *)

let parse_from_chan ic =

  let nat_of_char c = Char.code c - Char.code '0' in

  let rec scan_set ls optnum =
    match optnum, input_char ic with
    | None, (('0'..'9') as c) -> scan_set ls (Some (nat_of_char c))  
    | Some n, (('0'..'9') as c) -> scan_set ls (Some (n*10 + nat_of_char c))  
    | None, '}' -> ls
    | Some n, '}' -> n::ls
    | Some n, _ -> scan_set (n::ls) None
    | None, _ -> scan_set ls None
  in

  let rec scan acc =
    try 
      let c = input_char ic in
      ( match c with
        | '{' -> 
            let set = scan_set [] None in
            scan (set::acc)
        | _ -> scan acc
      )
    with
      End_of_file -> acc
  in
  
  scan [] |> ss_of_list

let parse_from_file file = 
  let ic = open_in file in
  let sc = parse_from_chan ic in
  close_in ic;
  sc


