open Printf
open Sc
open Common

let pdf oc w h draw_procedure = 
  (* Setup Cairo *)
  let surf = Cairo.PDF.create_for_stream (output_string oc) w h in
  let cx = Cairo.create surf in

  draw_procedure cx;

  Cairo.Surface.finish surf
       
let draw_poly cx conv ls =
  match ls with
  | hd::tl -> 
      let (x0,y0) = conv hd in
      Cairo.move_to cx x0 y0;
      List.iter (fun v -> 
        let x, y = conv v in
        Cairo.line_to cx x y
      ) tl;
      Cairo.line_to cx x0 y0
  | [] -> ()


(* PDF (Cairo) output  *)
let make_pdf oc sc pos =

  let nodes = Sc.nodes_set sc in

  let x0, y0, x1, y1 =
    if Sc.S.is_empty nodes then (0.0, 0.0, 10.0, 1.0)
    else
      Sc.S.fold (fun v (x0,y0,x1,y1) ->
          let x, y = pos v in
          (min x x0, min y y0, max x x1, max y y1)
        )
        nodes
        (max_float, max_float, -.max_float, -.max_float)
  in

  (* printf "x0 = %g, y0 = %g, x1 = %g, y1 = %g\n" x0 y0 x1 y1; *)

  let h = 100.0 in
  
  let margin = 0.3 in
  let double_margin = margin +. margin in

  let width = h*.(x1 -. x0 +. double_margin) in
  let height = h*.(y1 -. y0 +. double_margin) in
  
  (* printf "width = %g, height = %g\n" width height; *)
  
  let scalefx x = h *. (x -. x0 +. margin) in
  let scalefy y  = h *. (y -. y0 +. margin) in
  let scale (x,y) = (scalefx x, scalefy y) in


  pdf oc width height 
    (fun cx ->

      Cairo.set_line_width cx 1.0;
      Cairo.set_source_rgba cx 0.3 0.8 0.0 0.2;

      (* facets *)
      Sc.SS.iter (fun ft ->
        let ls = 
          if Sc.S.cardinal ft < 3 then
            ft |> Sc.S.elements |> List.map pos
          else
            Layout.convex_hull (ft |> Sc.S.elements |> List.map pos) 
        in

        let ls2 = 
          let circle n r center = 
            let two_pi = 8.0 *. atan 1.0 in
            let factor = two_pi /. float n in
            fold_lim (fun acc i ->
              let phi = float i *. factor in
              Layout.(center ++ (r *. cos phi, r *. sin phi)) :: acc
            )
            [] 0 (n-1) 
          in          
          ls |> List.map (fun xy -> circle 8 0.1 xy) |> List.flatten 
        in

        draw_poly cx scale (ls2 |> Layout.convex_hull);

        Cairo.fill cx;
      ) sc;

      Cairo.set_source_rgba cx 0.0 0.0 0.0 1.0;

      (* facet labels *)
      Sc.S.iter (fun v ->
        let (ox,oy) = v |> pos in
        (* printf "[%i](%g,%g)\n" v ox oy; *)
        let x,y = (ox, oy) |> scale in

        let text = string_of_int v in
        let fe = Cairo.font_extents cx in
        let te = Cairo.text_extents cx text in
        let xx = x -. te.Cairo.x_bearing -. te.Cairo.width /. 2.
        and yy = y -. fe.Cairo.descent +. fe.Cairo.baseline /. 2. in

        Cairo.move_to cx xx yy;
        Cairo.show_text cx (string_of_int v);

      ) nodes;

      Cairo.set_source_rgba cx 0.0 0.0 0.0 0.2;

    )

(* Plain text raw output *)    
let make_raw oc sc pos =
  
  let nodes = Sc.nodes_set sc in
  
  Sc.S.iter (fun v ->
    let (ox,oy) = v |> pos in
    printf "%i %g %g\n" v ox oy;
  ) nodes


let print_help () = print_endline 
"visualsc. A simplicial complex visualization tool similar to Graphviz. 

Usage examples:
  visualsc -i input.sc -o output.pdf
  visualsc < input.sc > output.pdf
  visualsc --raw [...]    
  visualsc --seed=136492834 [...]

Options:
  -i file
        input file (if not specified: read from stdin)
  -o file
        output file (if not specified: write to stdout)
  --raw
        print out node coordinates only (plain text output instead of PDF)
  --seed=integer
        specify PRNG seed (if not specified: chosen automatically)
"

let () =
  let open Opt in
  let flag, param, other_args =
    Opt.scan 
      (* flags  *) [Long "raw"; Short 'h'] 
      (* params *) [Short 'i'; Short 'o'; Long "seed"]
      Sys.argv 1
  in
  
  if flag (Short 'h') then 
    print_help ()
  
  else
  ( ( match param (Long "seed") with
      | Some (OK seed_s) -> Random.init(int_of_string(seed_s))
      | _ -> Random.self_init()
    );

    let ic, ic_close = 
      match param (Short 'i') with
      | Some (OK file) -> let ic = open_in file in (ic, fun () -> close_in ic)
      | _ -> stdin, (fun () -> ())
    in
    
    let oc, oc_close = 
      match param (Short 'o') with
      | Some (OK file) -> let oc = open_out file in (oc, fun () -> close_out oc)
      | _ -> stdout, (fun () -> ())
    in

    (* generate output *)
    let sc = Sc.parse_from_chan ic in
    ic_close ();

    let pos = Layout.layout sc in
    let make = if flag (Long "raw") then make_raw else make_pdf in
    make oc sc pos;

    oc_close();
  );

