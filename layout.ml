
open Common

module M = Map.Make(struct type t = int let compare = compare end)

let vector_product (a,b) (c,d) = a *. d -. b *. c 

let get key m = M.find key m
let upd key value m = M.add key value m

type vec = float * float
let (++) (a,b) (c,d) = (a+.c, b+.d)
let (--) (a,b) (c,d) = (a-.c, b-.d)
let (%%) x (a,b) = (x*.a, x*.b)
let (//) (a,b) x  = (a/.x, b/.x)

let ($) m k = get k m
let (>>) (k,v) m = upd k v m

let len2 (x,y) = x*.x +. y*.y 
let len (x,y) = hypot x y 

let layout initial_config sc = 

  (* se of nodes *)
  let nodes = Sc.nodes_set sc in

  (* map of initial zero locations for all nodes *)
  let zero_vec = Sc.S.fold (fun v acc -> (v,(0.0, 0.0)) >> acc) nodes M.empty in

  (* array of facet *)
  let af = Sc.SS.elements sc |> Array.of_list in
  
  (* array of "listified" facet *)
  let afl = af |> Array.map Sc.S.elements in
 

  (* Symmetric-difference-ed facets "listified" *)
  let fxorl = 
    let facets_num = Array.length af in
    Array.init facets_num (fun i ->
      Array.init facets_num (fun j ->
        let a = Sc.S.diff af.(i) af.(j) in
        let b = Sc.S.diff af.(j) af.(i) in
        match (a |> Sc.S.elements, b |> Sc.S.elements) with
        | [], [] -> None
        | [], _ 
        | _, [] -> failwith "Symmetric difference failure: One facet is a subset of another."
        | ls_a, ls_b -> Some (a, ls_a, Sc.S.cardinal a, b, ls_b, Sc.S.cardinal b)
      )
    )
  in

  (* node -> list of facet indices *)
  let in_facets = 
    let f v = 
      fold_lim (fun acc i -> if Sc.S.mem v af.(i) then i::acc else acc) [] 0 (Array.length af - 1) 
    in
    let m = Sc.S.fold (fun v acc -> (v, f v) >> acc) nodes M.empty in
    (fun v -> m $ v)
  in

  let compute_forces slowdown (mpos : vec M.t) (mvel : vec M.t) =

    (* pairwise "spring" interaction force *)
    let spring_force v =
      let vp = mpos$v in
      List.fold_left 
        ( fun sum fi ->
            List.fold_left 
              ( fun sum u ->
                  let disp = (mpos$u) -- vp in
                  let d = len disp +. 0.3 in
                  let z = (len disp -. 1.0) in
                  let df = 1.5 *. (z *. abs_float z +. z) %% (disp // d) in
                  sum ++ df
              )
              sum
              afl.(fi)
        )
        (0.0, 0.0)
        (in_facets v)
    in
      
    (* speed dissipation *)
    let dissip_force v =
      let vel = mvel$v in
      ((-1.0) +. (-1.0) *. (len vel)) %% vel 
    in
   
    (* overlap force *)
    let overlap_force =
      
      let find_center ft =
        let sum = Sc.S.fold (fun v sum -> sum ++ (mpos$v)) ft (0.0,0.0) in
        sum // (ft |> Sc.S.cardinal |> float) 
      in

      let find_radius center ft =
        ( Sc.S.fold (fun v acc -> max acc (len ((mpos$v) -- center))) ft 0.25 ) *. 1.0
      in

      let centers = Array.map find_center af in
      
      let radiuses = Array.mapi ( fun i ft -> find_radius centers.(i) ft ) af in
    
      let facets_num = Array.length af in

      let m = 
        fold_lim (fun mfacc i ->
          fold_lim (fun mfacc j ->
            let centeri, radi, li, ni, centerj, radj, lj, nj, they_intersect =
              match fxorl.(i).(j) with
              | Some (fti, li, ni, ftj, lj, nj) -> (* if facets i and j intersect *)
                  let ci = find_center fti in
                  let cj = find_center ftj in
                  (ci, find_radius ci fti, li, ni, cj, find_radius cj ftj, lj, nj, true)
              | None -> (* if don't intersect *)
                  centers.(i), radiuses.(i), afl.(i), Sc.S.cardinal af.(i), centers.(j), radiuses.(j), afl.(j), Sc.S.cardinal af.(j), false
            in

            let vec_ij = centerj -- centeri in
            let dist = len vec_ij in

            let overlap = radi +. radj -. dist +. 0.1 in
            
            let repel_disjoint = if not they_intersect then 1.0 else 0.0 in
            
            let overlap = overlap +. 0.3 *. (min 1.0 (0.1 /. (dist +. 0.001))) in

            let overlap = overlap +. 0.2 *. repel_disjoint in

            let overlap = overlap +. 0.4 *. (if overlap > 0.0 then 0.4 else max 0.0 (0.4 -. overlap)) in 
            (* let overlap = overlap +. 0.1 *. (if overlap > 0.0 then 1.0 else exp(4.0*.overlap) ) in *)
            
            if overlap > 0.0 then
            ( 
              let fni = float ni in
              let fnj = float nj in

              let dir = 
                if dist < 0.00001 then 
                  let phi = float (j-i) /. float (Sc.S.cardinal nodes) *. 6.2831853  in
                  (cos phi, sin phi)
                else
                  vec_ij // dist
              in

              let net_force = (-.overlap) *. 1.0 *. (2.0 +. sqrt(fni +. fnj)) %% dir in

              let m1 = List.fold_left (fun m v -> (v, ((m$v) ++ (1.0/.fni)%%net_force)) >> m) mfacc li in
              let m2 = List.fold_left (fun m v -> (v, ((m$v) ++ (-1.0/.fnj)%%net_force)) >> m) m1 lj in
              m2
            )
            else
              mfacc
          )
          mfacc
          (i+1) (facets_num-1)
        )
        zero_vec
        0 (facets_num-1)
      in

      (fun v -> m$v)
    in

    let gravity_force = 
      let sum = Sc.S.fold (fun v sum -> sum ++ (mpos$v)) nodes (0.0,0.0) in
      let center = sum // (nodes |> Sc.S.cardinal |> float) in
      let radius = Sc.S.fold (fun v acc -> max acc (len ((mpos$v) -- center))) nodes 0.0 in
      
      let number = Sc.S.cardinal nodes in
      let volume_per_node = (3.1415 *. radius *. radius) /. float number in
      
      (fun v -> 
          let length = len (center -- (mpos$v)) in
          (max 0.0 (volume_per_node -. 1.0)) *. (length /. radius) %% (center -- (mpos$v)) // radius 
      )
    in

    (* forces on all vertices *)
    M.mapi (fun v _ -> slowdown %% (spring_force v ++ overlap_force v ++ gravity_force v) ++ dissip_force v) mpos  
  in

  (* update one step *)
  let update dt slowdown (mpos, mvel) =
    let f = compute_forces slowdown mpos mvel in
    M.fold (fun v _ (mp, mv) ->
     
      let vel = mvel$v in
      let pos = mpos$v in
      let force = f$v in

      let upd_vel = vel ++ dt %% force in
      let upd_pos = pos ++ (dt*.0.5) %% (vel ++ upd_vel) in

      ((v, upd_pos) >> mp, (v,upd_vel) >> mv)

    ) mpos (mpos, mvel)
  in

  (* relaxation iteration *)
  let iter_relax dt t0 ms =
    let rec fold dt t ((mpos, mvel) as ms) =
      if t > 0.0 then
        fold dt (t-.dt) (update dt (t /. t0) ms)
      else
        ms
    in
    fold dt t0 ms
  in
  (* initial configuration *)
  let gen_mpos_initial () = 
    let n = Sc.S.cardinal nodes in
    let rnd = 
      let dim = sqrt (float n) in
      (fun () -> Random.float dim)
    in
    M.mapi (fun v _ -> 
        match initial_config v with
        | Some xy -> xy
        | None -> (rnd(), rnd())
      ) 
      zero_vec 
  in
  
  let mvel_initial = zero_vec in

  let mpos_initial =
    let gen () =
      let mpos = gen_mpos_initial () in
      let mf = compute_forces 1.0 mpos mvel_initial in
      let sum = M.fold (fun _ vec sum -> len vec +. sum) mf 0.0 in
      (mpos, sum)
    in

    let mpos, _ = 
      fold_lim (fun (m_acc, sum_acc) _ ->
        let mpos, sum = gen() in
        if sum < sum_acc then 
          (mpos, sum) 
        else 
          (m_acc, sum_acc)
      ) (gen()) 0 (5 * (Sc.S.cardinal nodes))
    in
    mpos
  in

  (* run relaxation iteration *)
  let time =
    let nodes_num = Sc.S.cardinal nodes in
    let facets_num = Array.length af in
    1.0 *. float (nodes_num + facets_num)
  in

  let nodes_num = Sc.S.cardinal nodes in
  
  let mpos = 
    if nodes_num > 1 then 
      let mpos, _ = iter_relax 0.01 time (mpos_initial, mvel_initial) in
      mpos
    else
      mpos_initial
  in

  (fun v -> mpos$v)


let convex_hull ls =
  (* Graham's algorithm *)
  let (x0,y0) as origin =
    List.fold_left (fun (accx, accy) (x,y) -> 
        if y < accy || (y = accy && x < accx) then (x,y)
        else (accx,accy)
      ) 
      (max_float, max_float) ls
  in

  (* Printf.printf "origin = %s\n" (s_of_loc origin); *)

  let sorted_ls = 
    let compare l1 l2 = 
      let ll1 = l1--origin in
      let ll2 = l2--origin in
      let c = compare (vector_product ll2 ll1) 0.0 in 
      if c = 0 then compare (len ll1) (len ll2) 
      else c
    in
    let arr = Array.of_list ls in
    Array.sort compare arr;
    Array.to_list arr 
  in

  (* List.iter (fun loc -> Printf.printf "%s\n" (s_of_loc loc)) sorted_ls; *)

  (* print_newline(); *)

  let rec reduce = function
    | z::y::x::tl as all ->
        let prod = vector_product (y--x) (z--x) in
        if prod <= 0.0 then 
          ( (* Printf.printf "\t\t\t%s - %s - %s\n" (s_of_loc z) (s_of_loc y) (s_of_loc x); *)
            reduce (z::x::tl) )
        else 
          all
    | ls -> ls
  in

  let head = List.hd sorted_ls in
  let rec next = function
    | acc, x::xs -> 
        (* 
        let print ls = List.iter (fun loc -> Printf.printf "%s" (s_of_loc loc)) ls in
        print (x::acc); print_endline " -> ";
        let r = reduce (x::acc) in
        print r; print_endline "\n";
        next (r,xs)
        *)
        next ((reduce (x::acc)), xs) 
    | acc, [] -> reduce (head::acc) |> List.tl
  in
  next ([], sorted_ls) |> List.rev

