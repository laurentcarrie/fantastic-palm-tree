open ExtList
open ExtString
open Printf
open Read_util
open Write_util

module D = Datamodel

let (//) = Filename.concat

let write_bar fout index (bar:D.Grille.bar) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
%% write bar
x1 := x0 + width ;
draw(x0,base_line) -- (x0,base_line+height) ;
" (* (List.length bar) *)   in

  let (has_position:bool) = 
    match bar.D.Grille.chords with
      | [] -> false
      | hd::tl -> (
	  List.fold_left ( fun has_position c ->
	    match has_position,c.D.Accord.position with
	      | false,None -> false
	      | true,Some _ -> true
	      | _,_ -> let msg = "mix of chords with position and without position" in
			 eprintf "%s\n" msg ;
		  failwith msg
	  ) (Option.is_some hd.D.Accord.position) tl
	)
  in
	    
  let e_of_chord_with_position pid i c nb = 
    let bid = sprintf "%s_%d" pid i in
    let ret = "" in
    match c.D.Accord.chord with
      | Some a -> (
	  let ret = sprintf "%s
boxit.%s_a(btex \\notefont{%c} etex) ;
boxit.%s_m(btex \\subscriptfont{%s} etex) ;
" ret bid a.D.Accord.note bid (if a.D.Accord.minor then "m" else "") in 
	  let (ret:string) = (if i mod 2 = 0 then 
	    sprintf "%s
%s_a.sw = %s.sw + 0.5 * (%s.nw-%s.sw) ;
%s_a.ne = %s.ne ;
%%fill bpath.%s_a withcolor (%s) ;
"
	      ret
	      bid pid pid pid
	      bid pid
	      bid ".8,.2,.1"
	    else
	      sprintf "%s
%s_a.sw = %s.sw ;
%s_a.ne = %s.ne - 0.5 * (%s.ne-%s.se) ;
"
		ret
		bid pid
		bid pid pid pid
	  )
	  in
	  let ret = sprintf "%s
drawunboxed(%s_a) ;
" 
	    ret
	    bid
(*
	  let rlap = sprintf "\\rlap{\\textsubscript{\\subscriptfont{%s}}}{\\textsuperscript{\\subscriptfont{%s}}}" sa usa in
	    sprintf "(%s%s etex,((x1-x0)*%d/%d,height*2.5/3)) ; \n" a rlap (Option.get c.D.Accord.position) nb
*)
	  in
	    (* majeur mineur *)
	  let (ret:string) = 
	    sprintf "%s
%s_m.se = %s_a.se ;
%s_m.nw = 0.2 * %s_a.nw + 0.8 * %s_a.se ;
drawunboxed(%s_m) ;
"
	      ret
	      bid bid
	      bid bid bid
	      bid
	  in
	    (* flat sharp *)
	  let (ret:string) = 
	    sprintf "%s
boxit.%s_fs(btex %s etex) ;
%s_fs.ne = %s_a.ne ;
%s_fs.sw = 0.2 * %s_a.nw + 0.8 * %s_a.se ;
drawunboxed(%s_fs) ;
"
	      ret
	      bid (match a.D.Accord.alteration with | D.Accord.None -> "" | D.Accord.Flat ->  "\\flatsharpfont{$\\flat$}" | D.Accord.Sharp ->"\\flatsharpfont{$\\sharp$}" )
	      bid bid
	      bid bid bid
	      bid
	  in
	    ret

	)
      | None   -> 
	  sprintf "(%s etex,(((x1-x0)*%d/%d,height*0.6))) ; \n" D.tex_silence (Option.get c.D.Accord.position) nb
  in

(*
  let e_of_chord_without_position c nb i = 
    match c.D.Accord.chord with
      | Some a -> 
	  let (a,sa,usa) = mp_of_chord a in
	  let rlap = sprintf "\\rlap{\\textsubscript{\\subscriptfont{%s}}}{\\textsuperscript{\\subscriptfont{%s}}}" sa usa in
	    sprintf "(%s%s etex,((x1-x0)*%d/%d,height*2.5/3)) ; \n" a rlap i nb 
      | None   -> 
	  sprintf "(btex %s etex,(((x1-x0)*%d/%d,height*0.6))) ; \n" D.tex_silence i nb 
  in
*)

(*
  let () = if has_position then (
    List.iter ( fun c ->
      pf "%s" (e_of_chord_with_position c 9)
    ) bar.D.Grille.chords 
  ) else (
    let nb =  (List.length bar.D.Grille.chords) + 1  in
    List.iteri ( fun i c ->
      pf "%s" (e_of_chord_without_position c nb  (i+1))
    ) bar.D.Grille.chords 
  )
  in
*)


  let () = pf "
%%   draw (x0,base_line) -- (x1,base_line) ;
.
%%   draw (x0,base_line+height) -- (x1,base_line+height) ;
boxit.b%d(btex %s etex) ;
b%d.sw=(x0,base_line) ;
b%d.ne=(x1,base_line+height) ;
%%fill bpath.b%d withcolor (.8,.2,.8) ;
drawboxed(b%d) ;
"
index  
""
index index index index in
  let () = if has_position then (
    let nb = 10 in
    List.iteri ( fun i c ->
      let pos = Option.get c.D.Accord.position in
      let (bid:string) = sprintf "b%d_%d" index i in
      pf "
%% has position
boxit.%s(btex etex) ;
%s.sw=(x0+(x1-x0)*(%d-1)/%d,base_line) ;
%s.ne=(x0+(x1-x0)*(%d)/%d,base_line+height) ;
drawunboxed(%s) ;
%s
" 
	bid
	bid (pos+1) nb
	bid (pos+1) nb
	bid
	(e_of_chord_with_position bid i c nb)
    ) bar.D.Grille.chords 
  ) else (
    (* let nb=9 in *)
    List.iteri ( fun i c ->
      pf "
%% has no position
boxit.b%d_%d(btex etex) ;
b%d_%d.sw=(x0+(x1-x0)*%d/%d,base_line) ;
b%d_%d.ne=(x0+(x1-x0)*(%d+1)/%d,base_line+height) ;
drawboxed(b%d_%d) ;
" 
	index i
(*	(e_of_chord_without_position c nb i)*)
	index i i (List.length bar.D.Grille.chords)
	index i i (List.length bar.D.Grille.chords)
	index i
    ) bar.D.Grille.chords
  ) in

  let () = pf "%s" "
x0:=x1 ;
" in
    ()
)



let write_line fout index (line:D.Grille.ligne) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
%% write line
  x0:=0 ;
"  in
  let () = List.iteri ( fun i b -> write_bar fout (index*100+i) b ) line.D.Grille.bars in

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
\\usepackage{latexsym}
\\usepackage{nicefrac}
\\usepackage{textcomp}
\\newcommand*{\\notefont}{\\fontfamily{ptm}\\fontsize{10}{10}\\selectfont}
\\newcommand*{\\subscriptfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}
\\newcommand*{\\flatsharpfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}
\\begin{document}
etex

input boxes ;
beginfig(1) ;
  uy=0.2cm ;
%%  ux=0.15cm ;
  unote=0.1cm ;
%%  b=10*uy ;
  base_line     = 0*uy ;
  gap_base_line = 0*uy ;
  height        = 4*uy ;
  width         = 2cm ;
%% scale pour le normal chord
  schord=1.3
%% scale pour le susbcript chord
  sschord=1 ;
  pickup pencircle scaled 0.15bp ;
  "  
  in 

    
  let () = List.iteri ( fun index l -> write_line fout index l ) grille.D.Grille.lignes in
 

    
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
