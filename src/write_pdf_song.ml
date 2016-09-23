open ExtList
open ExtString
open Printf
open Read_util
open Write_util


module D = Datamodel

let (//) = Filename.concat

let write_mp3 (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  (* pf "mp3 file : %s\n" title  *)
) ;;


let tex_of_string c = (
  List.fold_left ( fun c (sub,by) ->
    let reg = Str.regexp (Str.quote sub) in
      Str.global_replace reg by c
  ) c  [
    "#","$\\sharp$" ;
  ]
)
(*
let write_grille ~transpose fout (g:D.Grille.t) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let taille = List.fold_left ( fun t line ->
    let t2 = List.length line in
    if t > t2 then t else t2
  ) 0 g.D.Grille.lignes in

  pf "\

  \\centering
" ;
  pf "%%\\caption*{%s}\n" (tex_of_string g.D.Grille.titre) ;

  (* if g.Grille.titre <> "" then pf "{\\lyricstitlefont %s}\\\\\n\n" (tex_of_string g.Grille.titre) else () ; *)
  pf "\\begin{grillefont} \n" ;
  pf "\\begin{tabular}{|%s|}\n" (String.join "|" 
       (List.init taille (fun _ -> "M{2cm}"))
) ;
  let () = pf "\\multicolumn{%d}{c}{\\lyricstitlefont %s} \\\\\n" taille (tex_of_string g.D.Grille.titre)  in
  let length = List.fold_left ( fun previous_length line ->
    let l = Pervasives.max previous_length (List.length line) in
    pf "\\cline{1-%d}\n" l ;
    let tex_of_bar (b:D.Grille.e list) = 
      match b with 
	| D.Grille.A b -> (
	    let  l = List.map tex_of_chord b in
	    let s = match l with
	      | [] -> ""
	      | s::[] -> s
	      | a::b::[] -> 
		  (* sprintf "\\diagbox[dir=NE]{%s}{%s}" a b *)
		  sprintf "%s %s" a b
	      | l -> String.join " " l in
	      (* "\\tabbox[c]{" ^ s ^ "}"   *)
	      s
	  )
	| D.Grille.S -> (
	    "S"
	  )
    in
    let bars = List.map tex_of_bar line in
    pf "%s" (String.join " & " bars ) ;
    pf "%s" "\\\\\n" ;
    List.length line
  ) (-1) g.D.Grille.lignes in
  pf "\
  \\cline{1-%d}
  \\end{tabular}
  \\end{grillefont}


" length ;

) ;;
*)

let write_lyrics fout l = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let (nbcols,title,l) = l in
  let () = pf "\\begin{lyricsfont}\n" in 
  let () = pf "\\begin{verse}\n" in
  let () = pf "{\\commentfont \\hl{%s}} \n" (tex_of_string title) in  
  (* let () = pf "\\paragraph{\\commentfont \\hl{%s}} \n" title in  *)
  let () = List.iter ( fun line ->
    if line="\\" then (
      (* pf "%s" "\\newline\n"  *) ()
    )  else (
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "{\\sethlcolor{grey8}\\commentfont \\hl{\\1}}" line in

      let rec r line = (
	try
	  let reg = Pcre.regexp "\\[(.*?);(.*?)\\]" in 
	  let s = Pcre.exec ~rex:reg ~pos:0 line in
	  let chord = 
	    let l = Read_util.duration_and_silence_or_chord_of_string (Pcre.get_substring s 1) in
	      tex_of_chord l
	  in
	  let word = Pcre.get_substring s 2 in
	  let templ = sprintf "\\textsuperscript{\\textcolor{red}{%s}}\\underline{%s}" chord word in 
	  let line = Pcre.qreplace_first ~rex:reg ~templ:templ line in 
	  r line
	with
	| Not_found -> line
	| Pcre.Error e -> printf "pcre error\n" ; line
      ) in
      let line = r line in

      (* let line = tex_of_string line in  *)
      pf "%s\\\\\n" line
    )
  ) l in
  let () = pf "\\end{verse}\n" in
  let () = pf "\\end{lyricsfont}\n" in 
  let () = pf "\n" in
  ()
) 
let write_grille  song fout g name count = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let title = g.D.Grille.titre in
  let () = pf "
\\begin{tabular}{c}
\\\\
{\\commentfont \\hl{%s}}
\\\\
" (tex_of_string title) in

  let () = Write_mp_grille.write_mp song name g count in
    pf "
\\includegraphics{%s-grille-%d.mps}
\\end{tabular}
" (Filename.basename name) count ;
    ()
)

let write_tab  song fout tab name count = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let title = tab.D.Tablature.titre in
  let () = pf "
\\begin{tabular}{c}
\\\\
{\\commentfont \\hl{%s}}
\\\\
" (tex_of_string title) in

  let () = Write_mp_tab.write_mp song name tab count in
(*
  let () = List.iter ( fun line ->
    List.iter ( fun bar ->
      List.iter ( fun note ->
	(* pf "%d %d %d" note.D.Tablature.duration note.D.Tablature.frette note.D.Tablature.corde *)
	pf " "
      ) bar
    ) line
  ) tab.D.Tablature.lines
  in
*)
    pf "
\\includegraphics{%s-tab-%d.mps}
\\end{tabular}
" (Filename.basename name) count ;
    ()
)

let write_accords  fout l = (
)


let write_song_body fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = List.iter ( fun c ->
    match c with 
    | D.Normal s -> pf "%s\n" s
    | D.Titre _ 
    | D.Auteur _ -> ()
    | D.Grille g -> () (*write_grille ~transpose:song.Song.transpose fout name (g:Accord.t list list list)*)
    | D.Lyrics l -> () (* write_lyrics fout l *)
    | D.Mp3 l -> write_mp3 pf l
    | D.Tab tab -> () (* write_tab fout tab *)
    | D.Accords l -> write_accords fout l
    | D.Transpose i -> ()
    | D.Nb_croches _ -> ()
    | D.PageBreak -> pf "\\newpage\n"
  ) song.D.Song.data in


  let (grilles:D.Grille.t list) = List.rev (List.fold_left ( fun acc c -> match c with | D.Grille g -> g::acc | _ -> acc ) [] song.D.Song.data) in
  let () = pf "\\begin{multicols}{2}\n" in
  let name = Filename.chop_extension song.D.Song.filename in
  let () = List.iteri ( fun index g -> write_grille song fout g name index ) grilles in
  let () = pf "\\end{multicols}\n" in

  let (tabs:D.Tablature.t list) = List.rev (List.fold_left ( fun acc c -> match c with | D.Tab g -> g::acc | _ -> acc ) [] song.D.Song.data) in
  let () = 
    let () = pf "\\begin{multicols}{2}\n" in
    let name = Filename.chop_extension song.D.Song.filename in
    let () = List.iteri ( fun index tab -> write_tab song fout tab name index ) tabs in 
    let () = pf "\\end{multicols}\n" in
      ()
  in

  let lyrics = List.rev (List.fold_left ( fun acc c -> match c with | D.Lyrics l -> l::acc | _ -> acc ) [] song.D.Song.data) in
  let nbcols = List.fold_left ( fun acc (n,_,_) -> if n>acc then n else acc) 1 lyrics in
  let () = if nbcols>1 then pf "\\begin{multicols}{%d}\n" nbcols in
  let () = List.iter ( fun l -> write_lyrics fout l ) lyrics in
  let () = if nbcols>1 then pf "\\end{multicols}\n" in


  ()
)


let write_preamble fout  = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

    
  let _ = pf  "\
\\documentclass[a4paper,landscape]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
%%\\usepackage{wasysym}
\\usepackage{soulutf8}
\\usepackage{fixltx2e}
%%\\usepackage[pdftex,colorlinks=true,urlcolor=blue,linkcolor=blue]{hyperref}
\\usepackage{hyperref}
\\usepackage{diagbox}
%%\\usepackage{caption}
%%\\usepackage[scale=0.8]{geometry}
\\usepackage[a4paper,inner=1cm,outer=1cm,top=2cm,bottom=2cm]{geometry} 
\\usepackage{color,soul}
\\definecolor{grey}{rgb}{0.7,0.7,0.7}
\\definecolor{grey8}{rgb}{0.8,0.8,0.8}
\\sethlcolor{grey8}
\\usepackage[pdftex]{graphicx}
%%\\usepackage{lmodern}
%%\\usepackage{textcomp}
%%\\usepackage{kpfonts}
\\usepackage{array,multirow}
\\usepackage{fancyhdr} 
\\pagestyle{fancy} 
\\usepackage{makeidx}
\\newcolumntype{M}[1]{>{\\centering\\arraybackslash}m{#1}}
%%\\newcolumntype{M}[1]{>{\\centering}m{#1}}
\\newcolumntype{N}{@{}m{0pt}@{}}
\\renewcommand{\\arraystretch}{1.2}
\\usepackage{multicol}
%%\\usepackage{french}
\\setlength{\\columnseprule}{0.5pt}
\\usepackage{lastpage}
\\usepackage{needspace}
\\usepackage{titletoc}
\\hypersetup{ pdftitle={}, pdfauthor={},bookmarks=true, bookmarksopen=true,pdftoolbar=true, pdffitwindow=false,colorlinks=false,linkcolor=red, citecolor=red,filecolor=magenta,urlcolor=black }
%%\\usepackage{bookmark}
\\usepackage[metapost,truebbox,mplabels]{mfpic}
" in

(*
  let () = pf "\
\\fontencoding{T1}
\\fontfamily{garamond}
\\fontseries{m}
\\fontshape{it}
\\fontsize{22}{25}
\\selectfont
" 
in
*)
  
  let () = pf "
  \\newcommand\\invisiblesection[1]{%%
  \\refstepcounter{section}%%
  \\addcontentsline{toc}{section}{\\protect\\numberline{\\thesection}#1}%%
  \\sectionmark{#1}} \n" in
  let () = pf "\\newcommand*{\\authorfont}{\\fontfamily{ptm}\\fontsize{20}{25}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\titlefont}{\\fontfamily{ptm}\\fontsize{30}{35}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\commentfont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\lyricstitlefont}{\\color{black}\\fontfamily{ptm}\\fontsize{15}{15}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newenvironment{lyricsfont}{\\fontfamily{ptm}\\fontsize{15}{15}\\selectfont}{} \n" in
  let () = pf "\\newenvironment{grillefont}{\\color{blue}\\fontfamily{ptm}\\fontsize{15}{15}\\selectfont}{} \n" in 
  let () = pf "\\newcommand*{\\tabbox}[2][t]{%%
    \\vspace{0pt}\\parbox[#1][3.7\\baselineskip]{0.5cm}{\\strut#2\\strut}}\n" in
  (* let () = pf "\\newcommand{\\tstamp}{\\today}   \n" in *)
  (* let () = pf "\\fancyhead[C]{{\\titlefont %s} \\textsubscript{\\authorfont (%s)}} \n"  song.Song.titre song.Song.auteur in 
  let () = pf "\\fancyhead[R]{} \n" in
  let () = pf "\\fancyhead[L]{} \n" in
  *)
  
  let tm = Unix.localtime (Unix.time ()) in
  let () = pf "\\fancyfoot[L]{généré le %02d/%02d/%04d} \n" (tm.Unix.tm_mday)  (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900) in
  let () = pf "\\fancyfoot[C]{} \n" in
  let () = pf "\\fancyfoot[R]{page \\thepage~sur \\pageref{LastPage}} \n" in
  let () = pf "\\renewcommand\\headrulewidth{0.02in}" in 
  let () = pf "\\renewcommand\\footrulewidth{0.02in}" in 


  let () = pf "\\Needspace{5\\baselineskip}\n" in

  let () = pf "\\def\\mystrut(#1,#2){\\vrule height #1 depth #2 width 0pt}\n" in
  (* let () = pf "\\newcolumntype{C}{>{\\lower 2ex\\hbox to 6ex\\bgroup\\hss}c<{\\hss\\egroup}}\n" in *)
  let () = pf "\\newcolumntype{C}[1]{%%
   >{\\mystrut(3ex,2ex)\\centering}%%
   p{#1}%%
   <{}}  \n" in
  ()
)


let write_song fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = write_preamble fout in
  let _ = pf  "\
\\title{%s}
" song.D.Song.titre 
  in
  let () = pf "\\author{%s}\n" song.D.Song.auteur in

  let () = pf "\\fancyhead[L]{{\\titlefont %s} } \n"  song.D.Song.titre in 
  let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  song.D.Song.auteur in
  let () = pf "\\fancyhead[C]{} \n" in

  let () = pf "
\\begin{document}

" 
  in
  
  let () = write_song_body fout song in
  
  let () = pf "
\\end{document}
"
  in
  ()
)


let write_book ~book   = (
  let fout = open_out "book" ( ("book-"^(Filename.chop_suffix (Filename.basename book.D.Book.filename) ".book")^".tex")) in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    
  let () = write_preamble fout in
  let _ = pf  "\
\\title{%s}
" (Filename.basename book.D.Book.titre)
  in

  let () = pf "

\\makeatletter
\\let\\latexl@section\\l@section
\\def\\l@section#1#2{\\begingroup\\let\\numberline\\@gobble\\latexl@section{#1}{#2}\\endgroup}
\\makeatother
\\makeindex

\\begin{document} 
\\maketitle
%%\\begin{multicols}{2}
" in
  let () = pf "
%%\\end{multicolumns}
%%\\titlecontents{section}[0em]
%%{\\vskip 0.5ex}%%
%%{}%% numbered sections formattin
%%{}%% unnumbered sections formatting
%%{}%%

\\begin{multicols}{4}
\\tableofcontents{}
\\end{multicols}
" in
  let () = if book.D.Book.print_index then 
    pf "\\printindex\n" else 
      pf "%%printindex was not set\n"
  in
  let () = List.iter ( fun song ->
    match song with
    | D.Book.S song -> (
      let () = pf "\\clearpage\n" in
      let () = pf "\\index{%s!%s}" song.D.Song.auteur song.D.Song.titre in
      let () = pf "%%\\pdfbookmark[1]{%s}{%s}\n" song.D.Song.titre song.D.Song.titre in
      let () = pf "%%\\invisiblesection{%s}\n" song.D.Song.titre in
      let () = pf "\\fancyhead[L]{{\\invisiblesection{%s (%s)} \\titlefont %s} } \n" 
	song.D.Song.titre song.D.Song.auteur
	song.D.Song.titre in 
      let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  song.D.Song.auteur in
      let () = pf "\\fancyhead[C]{} \n" in
      write_song_body fout song
    )
    | D.Book.NF filename -> (
      let () = pf "\\clearpage\n" in 

      let () = pf "\\section{%s (non trouvé)}\n" filename in
      let () = pf "\\fancyhead[L]{{\\titlefont %s} } \n"  filename in
      let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  "" in 
      let () = pf "\\fancyhead[C]{} \n" in

      let () = pf "no such song : '%s'\n" filename in
      ()
    )
  ) book.D.Book.songs in

  let () = pf "
\\end{document}
" in
  let () = close_out fout in
  ()



)
