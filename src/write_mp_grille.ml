open ExtList
open ExtString
open Printf
open Read_util
open Write_util

module D = Datamodel

let (//) = Filename.concat

let write_bar fout (bar:D.Grille.bar) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
%% write bar
x1 := x0 + width ;
draw(x0,base_line) -- (x0,base_line+height) ;
" (* (List.length bar) *)   in

  let e_of_chord c n nb = 
    match c.D.Accord.chord with
      | Some a -> 
	  let (a,sa,usa) = mp_of_chord a in
	  let rlap = match sa,usa with
	    | "","" ->  ""
	    | sa,"" ->  sprintf "\\rlap{}{\\textsubscript{\\small{%s}}}" sa 
	    | "",usa ->  sprintf "\\rlap{\\textsuperscript{\\small{%s}}}{}" usa
	    | sa,usa -> sprintf "\\rlap{\\textsubscript{\\small{%s}}}{\\textsuperscript{\\small{%s}}}" sa usa
	  in
	    sprintf "label(btex %s%s etex,(x0+(x1-x0)*%d/%d,base_line+height/2)) ; \n" a rlap n nb
      | None   -> 
	  sprintf "label(btex \\textdagger etex,((x0+(x1-x0)*%d/%d,base_line+height/2))) ; \n" n nb
  in

  let () = match bar.D.Grille.chords with
    | [] -> (
	pf "%%empty bar\n" ; 
      )
    | c::[] -> (
	pf "%s" (e_of_chord c 1  2)
      )
    | c1::c2::[] -> (
	pf "%s" (e_of_chord c1 1  3) ;
	pf "%s" (e_of_chord c2 2  3)
      )
    | c1::c2::c3::c4::[] -> (
	pf "%s" (e_of_chord c1 1  5) ;
	pf "%s" (e_of_chord c2 2  5) ;
	pf "%s" (e_of_chord c3 3  5) ;
	pf "%s" (e_of_chord c4 4  5)
      )
    | l -> (
	pf "label(\"not managed %d\",((x1+x0)*2/3,base_line+2.5*uy)) ;\n" (List.length l) ;
      )
  in

  let () = pf "%s" "
   draw (x0,base_line) -- (x1,base_line) ;
   draw (x0,base_line+height) -- (x1,base_line+height) ;
   x0:=x1 ;
" in
    ()
)

let write_line fout (line:D.Grille.ligne) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
%% write line
  x0:=0 ;
"  in
  let () = List.iter ( fun b -> write_bar fout b ) line.D.Grille.bars in

  let () = pf "%s" "
draw(x1,base_line) -- (x1,base_line+height) ;
base_line := base_line - gap_base_line - height ;
" in
    ()
)


let  write_mp song name grille count  = (
  let filename = sprintf  "%s-grille-%d" (Filename.basename name) count in
  let fout = open_out "mp" (filename ^ ".mp") in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
verbatimtex 
%&latex
\\documentclass[12pt]{article}
\\usepackage{fixltx2e}
\\usepackage{amssymb}
\\usepackage{wasysym}
\\begin{document}
etex

beginfig(1) ;
  uy=0.2cm ;
  ux=0.25cm ;
  unote=0.1cm ;
%%  b=10*uy ;
  base_line     = 0*uy ;
  gap_base_line = 0*uy ;
  height        = 4*uy ;
  width         = 3cm ;
%% scale pour le normal chord
  schord=1.3
%% scale pour le susbcript chord
  sschord=1 ;
  pickup pencircle scaled 0.15bp ;
  "  
  in 

    
  let () = List.iter ( fun l -> write_line fout l ) grille.D.Grille.lignes in
 

    
  let () = pf "
endfig ;
\\end{document}
bye
" in
  let () = close_out fout 
  in
  let command = sprintf "mpost %s-grille-%d.mp > /dev/null " (Filename.basename name) count in
  let ret = Unix.system command in
  let () = match ret with
    | Unix.WEXITED 0 -> ()
    | _ -> let msg = sprintf "%s\nfailed" command in failwith msg
  in
  let target = sprintf "%s-grille-%d.1" (Filename.basename name) count in
  let () = if Sys.file_exists target then (
    let target2 =  sprintf "%s-grille-%d.mps" (Filename.basename name) count  in
      Sys.rename target target2
  )
    else
      failwith "mpost failed, cannot find target file"
  in
    ()
)
