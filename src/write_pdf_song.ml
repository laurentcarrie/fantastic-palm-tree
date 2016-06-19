open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat

let write_mp3 (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  (* pf "mp3 file : %s\n" title  *)
) ;;

let tex_of_chord (c:Accord.t) = (
  let s = sprintf "%c" c.Accord.note in
  let s = match c.Accord.alteration with
    | Accord.None -> s
    | Accord.Flat -> s ^ "\\textsuperscript{$\\flat$}"
    | Accord.Sharp -> s ^ "\\textsuperscript{$\\sharp$}"
  in
  let subscript = "" in
  let subscript = if c.Accord.minor then subscript^"m" else subscript in
  let subscript = if c.Accord.minor7 then subscript^"7" else subscript in
  let subscript = if c.Accord.major7 then subscript^"7M" else subscript in
  let s = if subscript="" then s else s^"\\textsubscript{"^subscript^"}" in
  let s = if s="%" then "" else ""^s^"" in
  s
)

let pdf_of_line c = (
  List.fold_left ( fun c (sub,by) ->
    let reg = Str.regexp (Str.quote sub) in
      Str.global_replace reg by c
  ) c  [
    "#","$\\sharp$" ;
  ]
)

let write_grille ~transpose fout name (g:Accord.t list list list) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let taille = List.fold_left ( fun t line ->
    let t2 = List.length line in
    if t > t2 then t else t2
  ) 0 g in

  pf "\
\\begin{center}
\\begin{table}[h!]
  \\centering
" ;
  pf "\\caption*{%s}\n" name ;
  pf "\\label{%s}\n" "" ;
  pf "\\begin{tabular}{|%s|}\n" (String.join "|" (List.init taille (fun _ -> "p{1.5cm}"))) ;
  pf "%%\\diagbox{i}{j} \\\\\\hline" ;

  let length = List.fold_left ( fun previous_length line ->
    let l = Pervasives.max previous_length (List.length line) in
    pf "\n\\cline{1-%d}\n" l ;
    let tex_of_bar (b:Accord.t list) = String.join " " (List.map tex_of_chord b) in
    let bars = List.map tex_of_bar line in
    pf "%s" (String.join " & " bars ) ;
    pf "%s" " \\\\" ;
    List.length line
  ) (-1) g in
  pf "
  \\cline{1-%d}
  \\end{tabular}
\\end{table}
\\end{center}
" length ;

) ;;

let write_lyrics fout l = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let (nbcols,title,l) = l in
  let () = pf "\\begin{verse}\n" in
  let () = pf "{\\commentfont \\hl{%s}} \n" title in  
  (* let () = pf "\\paragraph{\\commentfont \\hl{%s}} \n" title in  *)
  let () = List.iter ( fun line ->
    if line="\\" then (
      (* pf "%s" "\\newline\n"  *) ()
    )  else (
      let line = pdf_of_line line in 
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "{\\sethlcolor{grey8}\\commentfont \\hl{\\1}}" line in
      let reg = Str.regexp "\\[\\([^;]*\\);\\([^]]*\\)]" in
      let line = Str.global_replace reg "\\textsuperscript{\\1}\\2" line in
	pf "%s\\\\\n" line
    )
  ) l in
  let () = pf "\\end{verse}\n" in
  let () = pf "\n" in
  ()
) 

let write_tab  (pf : ('a, unit, string, unit) format4 -> 'a) l = (
)
(*
  pf "%s" "<div class=\"tablature\">\n<pre>" ;
  List.iter ( fun line ->
    if line="\\" then (
      pf "%s" "</p>\n<p class=\"tablature\">\n"
    )  else (
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "((\\1))" line in
	pf "%s\n" line
    )
  ) l ;
  pf "%s" "</p>\n" ;
  pf "%s" "</pre></div>\n" ; 
) 
*)

let write_accords  fout l = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    pf "\n<div class=\"chords\">\n" ;
    pf "\t<ul class=\"chords\">\n" ;
    List.iteri ( fun index l -> 
      pf "\t\t<li class=\"chords pos-%d\"><img class=\"chord\" src=\"png/%s.png\" alt=\"image %s pas trouvée\"/></li>\n" index l l ;
    ) l ;
    pf "\t</ul>\n" ;
    pf "</div>\n" ;
    pf "<div style=\"text-align:center; clear:both; margin-top:10px;border:1px solid white;\"></div>\n" ;    
)


let write_song_body fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = List.iter ( fun c ->
    match c with 
    | Normal s -> pf "%s\\newline\n" s
    | Titre _ 
    | Auteur _ -> ()
    | Grille (name,g) -> write_grille ~transpose:song.Song.transpose fout name (g:Accord.t list list list)
    | Lyrics l -> write_lyrics fout l
    | Mp3 l -> write_mp3 pf l
    | Tab l -> write_tab pf l
    | Accords l -> write_accords fout l
    | Transpose i -> ()
    | PageBreak -> pf "\\newpage\n"
  ) song.Song.data in

  ()
)


let write_preamble fout  = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

    
  let _ = pf  "\
\\documentclass[a4paper,portrait,twocolumn]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
%%\\usepackage{wasysym}
\\usepackage{soulutf8}
\\usepackage{fixltx2e}
\\usepackage{hyperref}
\\usepackage{diagbox}
\\usepackage{caption}
\\usepackage[scale=0.8]{geometry}
\\usepackage{color,soul}
\\definecolor{grey}{rgb}{0.7,0.7,0.7}
\\definecolor{grey8}{rgb}{0.8,0.8,0.8}
\\sethlcolor{grey8}
%%\\usepackage{lmodern}
%%\\usepackage{textcomp}
\\usepackage{kpfonts}
\\usepackage{array,multirow}
\\usepackage{fancyhdr} 
\\pagestyle{fancy} 
" in

  let () = pf "\
\\fontencoding{T1}
\\fontfamily{garamond}
\\fontseries{m}
\\fontshape{it}
\\fontsize{22}{25}
\\selectfont

" in

  let () = pf "\\newcommand*{\\authorfont}{\\fontfamily{ptm}\\fontsize{20}{25}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\titlefont}{\\fontfamily{ptm}\\fontsize{30}{35}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\commentfont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand*{\\lyricstitlefont}{\\fontfamily{ptm}\\fontsize{12}{15}\\fontshape{it}\\selectfont} \n" in
  let () = pf "\\newcommand{\\tstamp}{\\today}   \n" in
  (* let () = pf "\\fancyhead[C]{{\\titlefont %s} \\textsubscript{\\authorfont (%s)}} \n"  song.Song.titre song.Song.auteur in 
  let () = pf "\\fancyhead[R]{} \n" in
  let () = pf "\\fancyhead[L]{} \n" in
  *)
  
  let tm = Unix.localtime (Unix.time ()) in
  let () = pf "\\fancyfoot[L]{généré le %02d/%02d/%04d} \n" (tm.Unix.tm_mday)  (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900) in
  let () = pf "\\fancyfoot[C]{} \n" in
  let () = pf "\\fancyfoot[R]{page \\thepage} \n" in
  let () = pf "\\renewcommand\\headrulewidth{0.02in}" in 
  let () = pf "\\renewcommand\\footrulewidth{0.02in}" in 
  let () = pf "\\newcolumntype{C}{>{\\lower 2ex\\hbox to 6ex\\bgroup\\hss}c<{\\hss\\egroup}}\n" in
  ()
)


let write_song fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = write_preamble fout in
  let _ = pf  "\
\\title{%s}
" song.Song.titre 
  in
  let () = pf "\\author{%s}\n" song.Song.auteur in

  let () = pf "\\fancyhead[L]{{\\titlefont %s} } \n"  song.Song.titre in 
  let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  song.Song.auteur in
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


let write_book book songs = (
  let fout = open_out ( "tmp" // ("book-"^(Filename.chop_suffix (Filename.basename book.Book.filename) ".book")^".tex")) in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    
  let () = write_preamble fout in
  let _ = pf  "\
\\title{%s}
" (Filename.basename book.Book.titre)
  in

  let () = pf "
\\begin{document} 
\\maketitle
\\tableofcontents
" in
  let songs = List.fold_left ( fun acc b ->
    try 
      let song = List.find ( fun s -> s.Song.titre = b ) songs  in
      (b,Some song)::acc
    with
    | Not_found -> (b,None)::acc
  ) [] book.Book.songs in

  let () = List.iter ( fun (name,song) ->
    match song with
    | Some song -> (
      let () = pf "\\clearpage\n" in
      let () = pf "\\section{%s}\n" song.Song.titre in
      let () = pf "\\fancyhead[L]{{\\titlefont %s} } \n"  song.Song.titre in 
      let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  song.Song.auteur in
      let () = pf "\\fancyhead[C]{} \n" in
      write_song_body fout song
    )
    | None -> (
      let () = pf "\\clearpage\n" in 

      let () = pf "\\section{%s (non trouvé)}\n" name in
      let () = pf "\\fancyhead[L]{{\\titlefont %s} } \n"  name in
      let () = pf "\\fancyhead[R]{{\\authorfont %s}} \n"  "" in 
      let () = pf "\\fancyhead[C]{} \n" in

      let () = pf "no such song : '%s'\n" name in
      ()
    )
  ) songs in

  let () = pf "
\\end{document}
" in
  let () = close_out fout in
  ()



)
