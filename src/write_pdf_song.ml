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
    | Accord.Flat -> s ^ "^{\\flat}"
    | Accord.Sharp -> s ^ "^{\\sharp}"
  in
  let subscript = "" in
  let subscript = if c.Accord.minor then subscript^"m" else subscript in
  let subscript = if c.Accord.minor7 then subscript^"7" else subscript in
  let subscript = if c.Accord.major7 then subscript^"7M" else subscript in
  let s = if subscript="" then s else s^"_{"^subscript^"}" in
  let s = if s="%" then "" else "$"^s^"$" in
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
  pf "\\caption{%s}\n" name ;
  pf "\\label{%s}\n" "" ;
  pf "\\begin{tabular}{|%s|}\n" (String.join "|" (List.init taille (fun _ -> "p{1.5cm}"))) ;

  List.iter ( fun line ->
    pf "%s" "\n\\hline\n" ;
    let tex_of_bar (b:Accord.t list) = String.join " " (List.map tex_of_chord b) in
    let bars = List.map tex_of_bar line in
    pf "%s" (String.join " & " bars ) ;
    pf "%s" " \\\\"
  ) g ;
  pf "
  \\hline
  \\end{tabular}
\\end{table}
\\end{center}
" ;

) ;;

let write_lyrics fout l = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let (nbcols,l) = l in
  let () = pf "\\paragraph{xxx} \n" in 
  let () = List.iter ( fun line ->
    if line="\\" then (
      pf "%s" "\\newline\n" 
    )  else (
      let line = pdf_of_line line in 
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "((\\1))" line in
      let reg = Str.regexp "\\[\\([^;]*\\);\\([^]]*\\)]" in
      let line = Str.global_replace reg "((\\1))((\\2)" line in
	pf "%s\\newline\n" line
    )
  ) l in
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

let write_song fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = printf "write song %s\n" song.Song.titre in
    
  let _ = pf  "\
\\documentclass{article}
\\title{%s}
" song.Song.titre 
  in
  let () = pf "\\author{%s}\n" song.Song.auteur in
  let () = pf "\
\\usepackage[utf8]{inputenc}
\\usepackage{array}
\\usepackage{fancyhdr} 
\\pagestyle{fancy} 
" in
  let () = pf "\\newcommand{\\tstamp}{\\today}   \n" in
  let () = pf "\\fancyhead[C]{%s / %s} \n" song.Song.auteur song.Song.titre in
  let () = pf "\\fancyhead[R]{} \n" in
  let () = pf "\\fancyhead[L]{} \n" in
  let tm = Unix.localtime (Unix.time ()) in
  let () = pf "\\fancyfoot[L]{généré le %02d/%02d/%04d} \n" (tm.Unix.tm_mday)  (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900) in
  let () = pf "\\fancyfoot[C]{} \n" in
  let () = pf "\\fancyfoot[R]{page \\thepage} \n" in
  let () = pf "\\renewcommand\\headrulewidth{0.02in}" in 
  let () = pf "\\renewcommand\\footrulewidth{0.02in}" in 
  let () = pf "\\newcolumntype{C}{>{\\lower 2ex\\hbox to 6ex\\bgroup\\hss}c<{\\hss\\egroup}}\n" in
  let () = pf "
\\begin{document}
" 
  in

(*
  let () = pf "\\lfoot[\\fancyplain{}]{%4d-%02d-%02d}\n" (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) (tm.Unix.tm_mday) in
*)

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

  let () = pf "
\\end{document}
"
  in
  ()
)
