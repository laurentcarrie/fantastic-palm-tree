open ExtList
open ExtString
open Printf
open Read_util
open Write_util

module D = Datamodel

let (//) = Filename.concat

let write_bar fout bar = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "
%% write bar
x1 := x0 + 2*ux + %d*ux ;
draw(x0,base_line) -- (x0,base_line+5*1uy) ;
" (* (List.length bar) *) 8  in

  let () = match (List.map mp_of_chord bar) with
    | (a,sa,usa)::[] ->  (
	pf "label(btex %s etex scaled schord,(x0+(x1-x0)*1/2,base_line+2.5*uy)) ;\n" a ;
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*1/2,base_line+1.5*uy)) ;\n" sa ; 
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*1/2,base_line+3.5*uy)) ;\n" usa ; 
      )
    | (a,sa,usa)::(b,sb,usb)::[] -> (
	pf "label(btex %s etex scaled schord,(x0+(x1-x0)*1/3,base_line+2.5*uy)) ;\n" a ;
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*1/3,base_line+1.5*uy)) ;\n" sa ; 
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*1/3,base_line+3.5*uy)) ;\n" usa ; 

	pf "label(btex %s etex scaled schord,(x0+(x1-x0)*2/3,base_line+2.5*uy)) ;\n" b ;
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*2/3,base_line+1.5*uy)) ;\n" sb ; 
	pf "label.rt(btex %s etex scaled sschord,(x0+0*ux+(x1-x0)*2/3,base_line+3.5*uy)) ;\n" usb ; 
      )
    | _ -> (
	pf "label(\"not managed\",((x1+x0)*2/3,base_line+2.5*uy)) ;\n" ;
      )
  in

  let () = pf "%s" "
   i:=0 ;
   draw (x0,base_line+i*1uy) -- (x1,base_line+i*1uy) ;
   i:=5 ;
   draw (x0,base_line+i*1uy) -- (x1,base_line+i*1uy) ;
x0:=x1 ;
" in
    ()
)

let write_line fout line = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
%% write line
  x0:=0 ;
"  in
  let () = List.iter ( fun b -> write_bar fout b ) line in

  let () = pf "%s" "
draw(x1,base_line) -- (x1,base_line+5*1uy) ;
base_line := base_line - 6*ux ;
" in
    ()
)


let  write_mp song name grille count  = (
  let filename = sprintf  "%s-grille-%d" (Filename.basename name) count in
  let fout = open_out "mp" (filename ^ ".mp") in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
input TEX;
TEXPRE(\"%%&latex\" & char(10) & \"\\documentclass{article}\\begin{document}\\\\usepackage[utf8]{inputenc}\");
TEXPOST(\"\\end{document}\") ;
beginfig(1) ;
  uy=0.2cm ;
  ux=0.25cm ;
  unote=0.1cm ;
  b=10*u ;
  base_line=0*u ;
  line_offset=10*u ;
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
bye
" in
  let () = close_out fout 
  in
  let command = sprintf "mpost %s-grille-%d.mp > /dev/null " (Filename.basename name) count in
  let ret = Unix.system command in
  let () = match ret with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "mpost failed"
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
