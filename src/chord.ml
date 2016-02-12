type barre = { from_string : int ; to_string : int ; frette : int }

type w =
    | Finger of int
    | Mute
    | Open
    | In_barre of barre

type t = {
  b : barre option ;
  s1 : w ;
  s2 : w ;
  s3 : w ;
  s4 : w ;
  s5 : w ;
  s6 : w ;
}


type c = {
  name : string ;
  filename : string ;
  fingers : t ;
}

let chord_names = ["E";"F";"F#";"G";"G#";"A";"A#";"B";"C";"C#";"D";"D#" ]
let chord_filenames = List.map ( fun s ->  let s = Str.global_replace (Str.regexp (Str.quote "#")) "sharp" s in s ) chord_names

let e_form f = 
  let name = List.nth chord_names (f mod 12) in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "-e-form" 
  in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = In_barre b ;
	s5 = Finger (2+f) ;
	s4 = Finger (2+f) ;
	s3 = Finger (1+f) ;
	s2 = In_barre b ;
	s1 = In_barre b ;
      }
    }

let e7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7-e-form" 
  in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = In_barre b ;
	s5 = Finger (2+f) ;
	s4 = Finger (f) ;
	s3 = Finger (1+f) ;
	s2 = In_barre b ;
	s1 = In_barre b ;
      }
    }

let em_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m-e-form" 
  in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = In_barre b ;
	s5 = Finger (2+f) ;
	s4 = Finger (2+f) ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = In_barre b ;
      }
    }

let em7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m7-e-form" 
  in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = In_barre b ;
	s5 = Finger (2+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = In_barre b ;
      }
    }

let e7M_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7M" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7M-e-form" 
  in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = In_barre b ;
	s5 = Finger (2+f) ;
	s4 = Finger (1+f) ;
	s3 = Finger (1+f) ;
	s2 = In_barre b ;
	s1 = In_barre b ;
      }
    }

let c_offset f = if f>7 then f-8 else f+4 

let c_form f = 
  let name = List.nth chord_names (f mod 12) in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "-c-form" 
  in
  let f = c_offset f in
  let  b = { from_string=3;to_string=1;frette=(f) } in
    { 
      name = name ;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = Finger (3+f) ;
	s4 = Finger (2+f) ;
	s3 = In_barre b ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    } 
      
let cm_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m-c-form" 
  in
  let f = c_offset f in
  let  b = { from_string=3;to_string=1;frette=(f) } in
    { 
      name = name ;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = Finger (3+f) ;
	s4 = Finger (1+f) ;
	s3 = In_barre b ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    } 
      
let cm7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m7-c-form" 
  in
  let f = c_offset f in
  let  b = { from_string=3;to_string=1;frette=(f) } in
    { 
      name = name ;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = Finger (3+f) ;
	s4 = Finger (1+f) ;
	s3 = In_barre b ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    } 

let c7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7-c-form" 
  in
  let f = c_offset f in
    { 
      name = name ;
      filename = filename ;
      fingers = {
	b = None ;
	s6 = Mute ;
	s5 = Finger (3+f) ;
	s4 = Finger (2+f) ;
	s3 = Finger (3+f) ;
	s2 = Finger (1+f) ;
	s1 = Mute ;
      }
    } 
      
let c7M_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7M" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7M-c-form" 
  in
  let f = c_offset f in
  let  b = { from_string=3;to_string=1;frette=(f) } in
    { 
      name = name ;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = Finger (3+f) ;
	s4 = Finger (1+f) ;
	s3 = Finger (3+f) ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    } 
      

let a_offset f = if f>4 then f-5 else f+7 

let a_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "-a-form" 
  in
  let f= a_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = In_barre b ;
	s4 = Finger (2+f) ;
	s3 = Finger (2+f) ;
	s2 = Finger (2+f) ;
	s1 = In_barre b ;
      }
    }

let am_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m-a-form" 
  in
  let f= a_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = In_barre b ;
	s4 = Finger (2+f) ;
	s3 = Finger (2+f) ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    }

let am7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m7-a-form" 
  in
  let f= a_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = In_barre b ;
	s4 = Finger (2+f) ;
	s3 = In_barre b ;
	s2 = Finger (1+f) ;
	s1 = In_barre b ;
      }
    }

let a7M_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7M" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7M-a-form" 
  in
  let f= a_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = In_barre b ;
	s4 = Finger (2+f) ;
	s3 = Finger (1+f) ;
	s2 = Finger (2+f) ;
	s1 = In_barre b ;
      }
    }

let a7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7-a-form" 
  in
  let f= a_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Mute ;
	s5 = In_barre b ;
	s4 = Finger (2+f) ;
	s3 = In_barre b ;
	s2 = Finger (2+f) ;
	s1 = In_barre b ;
      }
    }


let g_offset f = if f>2 then f-3 else f+9

let g_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "-g-form" 
  in
  let f= g_offset f in
  let  b = { from_string=5;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Finger (3+f) ;
	s5 = Finger (2+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = Finger (3+f) ;
      }
    }

let gm_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m-g-form" 
  in
  let f= g_offset f in
  let  b = { from_string=5;to_string=2;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Finger (3+f) ;
	s5 = Finger (1+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = Finger (3+f) ;
	s1 = Mute ;
      }
    }

let gm7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "m7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "m7-g-form" 
  in
  let f= g_offset f in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Finger (3+f) ;
	s5 = Finger (2+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = Finger (3+f) ;
      }
    }

let g7_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7-g-form" 
  in
  let f= g_offset f in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Finger (3+f) ;
	s5 = Finger (2+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = Finger (3+f) ;
      }
    }

let g7M_form f = 
  let name = List.nth chord_names (f mod 12) in
  let name = name ^ "7M" in
  let filename = 
    let s = List.nth chord_filenames (f mod 12) in
      s ^ "7M-g-form" 
  in
  let f= g_offset f in
  let  b = { from_string=6;to_string=1;frette=f } in
    {
      name = name;
      filename = filename ;
      fingers = {
	b = Some b ;
	s6 = Finger (3+f) ;
	s5 = Finger (2+f) ;
	s4 = In_barre b ;
	s3 = In_barre b ;
	s2 = In_barre b ;
	s1 = Finger (3+f) ;
      }
    }



      
open Printf
      

let write_svg filename c = (
  let frette_1 = List.fold_left ( fun acc t ->
    match t with
      | Finger f -> if f<acc then f else acc
      | Mute | Open -> acc
      | In_barre b -> if b.frette<acc then b.frette else acc
  ) 30 [ c.fingers.s6 ; c.fingers.s5 ; c.fingers.s4 ; c.fingers.s3 ; c.fingers.s2 ; c.fingers.s1 ]
  in

  let frette_1 = if frette_1 = 0 then 1 else frette_1 in
  let frette_2 = frette_1 + 5 in

  let fout = open_out filename in
  let pf fs = ksprintf (fun s -> fprintf fout "%s" s ) fs in



  let frette_offset = frette_1 - 1 in

  let nb_hor=frette_2-frette_1+1 in
  let offset_h = 100 in
  let step_h = 20 in

  let nb_vert= 6 in
  let offset_v = 100 in
  let step_v = 60 in

  let height = offset_v + (nb_vert+1) * step_v in
  let width = offset_h + (nb_hor+1) * step_h in

  let () = pf "<svg width=\"%d\" height=\"%d\" >\n" width height in

    
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

  let put_barre c1 c2 frette =
    let frette = frette - frette_offset in
    let x = offset_h - step_h / 2 + ( 6-c1) * step_h in
    let y = offset_v + (frette-1)*step_v + step_v / 3 in
    let width = step_h * (c1-c2+1) in
    let height = step_v / 3 in
    let rx = step_h/3 in
    let ry = step_v/3 in
      pf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" ry=\"%d\" style=\"fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)\" />\n"
	x y width height rx ry
  in

  let put_fret_numbers offset  =
    let rec put i =
      let x = offset_h * 2 / 3 in
      let y = offset_v + i * step_v - step_v / 2 in
	pf "<text x=\"%d\" y=\"%d\" fill=\"black\">%d</text>\n"
	  x y (i+offset) ;
	if i<(nb_hor-1) then put (i+1) else ()
    in
      put 1
  in

  let put_open s =
    let x = offset_h + (6-s) * step_h in
    let y = offset_v * 4 / 5 in
      pf "<text x=\"%d\" y=\"%d\" fill=\"black\">0</text>\n"
	x y  
  in

  let put_mute s =
    let x = offset_h + (6-s) * step_h in
    let y = offset_v * 4 / 5 in
      pf "<text x=\"%d\" y=\"%d\" fill=\"black\">X</text>\n"
	x y  
  in

  let put_finger s c  = 
    match c with
      | Finger frette -> if frette>0 then put_barre s s frette else put_open s
      | Mute -> put_mute s 
      | Open -> put_open s
      | In_barre b -> if b.frette=0 then put_open s else ()
  in

  let put_nacre offset frette =
    let frette = frette - offset in
    let c1 = 5 in
    let c2 = 2 in
    let x = offset_h - step_h / 2 + ( 6-c1) * step_h in
    let y = offset_v + (frette-1)*step_v + step_v / 4 in
    let width = step_h * (c1-c2+1) in
    let height = step_v / 2 in
    let rx = 0 in
    let ry = 0 in
    let r=255 in let g=191 in let b=128 in
    let rs=255 in let gs=191 in let bs=128 in
      pf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"%d\" ry=\"%d\" style=\"fill:rgb(%d,%d,%d);stroke-width:3;stroke:rgb(%d,%d,%d)\" />\n"
	x y width height rx ry r g b rs gs bs
  in

  let put_name () =
    let x = offset_h + 2 * step_h in
    let y = offset_v * 2 / 3 in
      pf "<text x=\"%d\" y=\"%d\" fill=\"black\" style=\"font-family:Verdana;font-size:24; visibility:visible\">%s</text>\n"
	x y c.name 
  in

    
  let nacre =  [ 3 ; 5 ; 7; 9 ; 12 ; 15 ]  in
  let nacre = List.filter ( fun f -> f>=frette_1 && f<=frette_2-1) nacre in

  let () = List.iter ( fun frette -> put_nacre frette_offset frette ) nacre in
  let () = draw_lignes_verticales () in
  let () = draw_lignes_horizontales () in


  let () = Option.may ( fun b ->
    if b.frette > 0 then
      put_barre b.from_string b.to_string b.frette
    else
      ()
  ) c.fingers.b in
    
(*
  let () = put_barre 6 1 2 in
  let () = put_barre 4 1 3 in
  let () = put_barre 2 2 5 in
*)
  let () = put_fret_numbers frette_offset in
  let () = put_name () in

  let () = put_finger 6 c.fingers.s6 in
  let () = put_finger 5 c.fingers.s5 in
  let () = put_finger 4 c.fingers.s4 in
  let () = put_finger 3 c.fingers.s3 in
  let () = put_finger 2 c.fingers.s2 in
  let () = put_finger 1 c.fingers.s1 in

  let () = pf "%s" "
</svg>
" 
  in
  let () = close_out fout in

    

    ()
)    

  
  
