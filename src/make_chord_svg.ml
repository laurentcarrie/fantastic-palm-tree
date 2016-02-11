open Printf

let print_chord fout  = (
  let pf fs = ksprintf (fun s -> fprintf fout "%s" s ) fs in

  let () = pf "%s"
"
<svg width=\"1000\" height=\"1000\">
" in

  let nb_hor=10 in
  let offset_h = 100 in
  let step_h = 20 in

  let nb_vert= 7 in
  let offset_v = 100 in
  let step_v = 60 in
    
  let draw_lignes_verticales () = 
    let rec print_line x i =
      let y1 = offset_h in
      let y2= offset_h + (nb_hor-1) * step_v in
      let _ = pf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" x y1 x y2 in
	if i>(nb_vert-2) then () else
	  print_line (x+step_h) (i+1)
    in
      print_line offset_v 0
  in

  let draw_lignes_horizontales () = 
    let rec print_line y i =
      let x1=offset_v in
      let x2=offset_v + (nb_vert-1) * step_h in
      let _ = pf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"1\"/>\n" x1 y x2 y in
	if i>(nb_hor-2) then () else
	  print_line (y+step_v) (i+1)
    in
      print_line offset_h 0
  in
    
  let () = draw_lignes_verticales () in
  let () = draw_lignes_horizontales () in

let () = pf "%s" "
</svg>
" 
  in
    ()
)    

let main () =
  let out_filename = Sys.argv.(1) in
  let () = printf "Writing to %s\n" out_filename in
  let fout = open_out out_filename in
  let () = print_chord fout in
  let () = close_out fout in
    ()

let _ = 
  main ()
    
