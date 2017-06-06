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

  let e_of_chord_without_position pid c nb i =
    let ret = "" in
    match c.D.Accord.chord with
      | Some a -> (
	  let ret = sprintf "%s
%% %s:%d
boxit.%s_n(btex \\notefont{%c} etex) ;
%s_n.c = %s.c ;
drawunboxed(%s_n) ;
"
	    ret
	    __FILE__ __LINE__
	    pid  a.D.Accord.note
	    pid pid
	    pid
	  in
	    (* minor major *)
	  let ret = sprintf "%s
boxit.%s_m(btex \\subscriptfont{%s} etex) ;
%s_m.c = %s_n.se ;
drawunboxed(%s_m) ;
"
	    ret 
	    pid (if a.D.Accord.minor then "m" else "")
	    pid pid
	    pid
	  in

	    (* alteration *)
	  let ret = sprintf "%s
boxit.%s_fs(btex \\subscriptfont{%s} etex) ;
%s_fs.c = %s_n.ne ;
drawunboxed(%s_fs) ;
"
	    ret 
	    pid (match a.D.Accord.alteration with | D.Accord.None -> "" | D.Accord.Flat ->  "\\flatsharpfont{$\\flat$}" | D.Accord.Sharp ->"\\flatsharpfont{$\\sharp$}" )
	    pid pid
	    pid
	  in


	    (* accord *)
	  let aa = "" in
	  let aa = aa^(if a.D.Accord.minor7 then "\\subscriptfont{7}" else "") in
	  let aa = aa^(if a.D.Accord.major7 then "\\trianglefont{$\\bigtriangleup$}" else "") in
	  let aa = aa^(if a.D.Accord.sus4 then "\\subscriptfont{4}" else "") in
	  let ret = if aa="" then ret else (sprintf "%s
boxit.%s_7(btex %s etex) ;
%s_7.c =  %s_n.e ;
drawunboxed(%s_7) ;
"
	    ret 
	    pid aa
	    pid pid
	    pid
	  )
	  in

	    ret
	)
      | None ->
	  ""
  in
	    
  let e_of_chord_with_position pid i c nb = 
    let bid = sprintf "%s_%d" pid i in
    let ret = "" in
    match c.D.Accord.chord with
      | Some a -> (
	  let ret = sprintf "%s
%% %s:%d
boxit.%s_a(btex \\notefont{%c} etex) ;
boxit.%s_m(btex \\subscriptfont{%s} etex) ;
" ret 
__FILE__ __LINE__
bid a.D.Accord.note bid (if a.D.Accord.minor then "m" else "") in 
	  let (ret:string) = (
	    sprintf "%s
%s_a.sw = %s.sw ;
%s_a.ne = %s.ne ;
%%fill bpath.%s_a withcolor (%s) ;
"
	      ret
	      bid pid 
	      bid pid
	      bid ".8,.2,.1"
	  )
	  in
	  let ret = sprintf "%s
drawboxed(%s_a) ;
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


  let () = pf "
%%   draw (x0,base_line) -- (x1,base_line) ;

%%   draw (x0,base_line+height) -- (x1,base_line+height) ;
%% %s:%d
boxit.b%d() ;
b%d.sw=(x0,base_line) ;
b%d.ne=(x1,base_line+height) ;
%%fill bpath.b%d withcolor (.8,.2,.8) ;
drawboxed(b%d) ;
"
__FILE__ __LINE__
index  
index index index index in
    (*
    pf "
boxit.bb_%d(btex %d etex) ;
bb_%d.sw = (x0,base_line) ;
drawboxed(bb_%d) ;
" index index index index ;
    *)
  let () = if has_position then (
    let nb = 4.5 in
    List.iteri ( fun i c ->
      let pos = Option.get c.D.Accord.position in
      let (bid:string) = sprintf "b%d_%d" index i in
	pf "
%% has position
%% %s:%d
boxit.%s() ;
" 
	  __FILE__ __LINE__
	  bid ;

	if i mod 2 = 0 then (
	  pf "
%s.sw=(x0+(x1-x0)*(%d/2-0.5)/%f,base_line+height/2) ;
%s.ne=(x0+(x1-x0)*(%d/2+0.5)/%f,base_line+height) ;
" 
	    bid pos nb
	    bid pos nb
)
	else (
	    pf"
%s.sw=(x0+(x1-x0)*(%d/2-0.5)/%f,base_line) ;
%s.ne=(x0+(x1-x0)*(%d/2+0.5)/%f,base_line+height/2) ;
"
	      bid pos nb
	      bid pos nb
	) ;
	
	pf "
drawunboxed(%s) ;
%s
" 
	  bid
	  (e_of_chord_with_position bid i c (int_of_float nb))
    ) bar.D.Grille.chords 
  ) else (
    let nb = List.length bar.D.Grille.chords in
    List.iteri ( fun i c ->
      let bid = sprintf "b%d_%d" index i in
      pf "
%% %s:%d
%% has no position
boxit.%s() ;
x00 := x0 + 3 ;
x11 := x1 - 3 ;
%s.sw=(x00+(x11-x00)*%d/%d,base_line) ;
%s.ne=(x00+(x11-x00)*(%d+1)/%d,base_line+height) ;
drawunboxed(%s) ;
%s
" 
	__FILE__ __LINE__
	bid
	bid i nb
	bid i nb
	bid
	(e_of_chord_without_position bid c nb i)
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
\\newcommand*{\\notefont}{\\fontfamily{ptm}\\fontsize{12}{12}\\selectfont}
\\newcommand*{\\subscriptfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}
\\newcommand*{\\flatsharpfont}{\\fontfamily{ptm}\\fontsize{8}{8}\\fontshape{it}\\selectfont}
\\newcommand*{\\trianglefont}{\\fontfamily{ptm}\\fontsize{4}{4}\\selectfont}
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
