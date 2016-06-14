open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat

let write_mp3 (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<div class=\"mp3\"><a href=%s>./%s</a></div>" title title
) ;;


  

let html_of_chord (c:Accord.t) = (
  let s = sprintf "%c" c.Accord.note in
  let s = if c.Accord.minor then s^"<sub>m</sub>" else s in
  let s = match c.Accord.alteration with
    | Accord.None -> s
    | Accord.Flat -> s ^ "&#x266d;"
    | Accord.Sharp -> s ^ "&#x266f;"
  in
  let s = if c.Accord.minor7 then s^"7" else s in
  let s = if c.Accord.major7 then s^"7M" else s in
  s
)

let write_grille ~transpose fout name (g:Accord.t list list list) = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    pf "<div class=\"grille\">\n" ;
    pf "\n<table>\n" ;
    List.iter ( fun line ->
      pf "<tr>"  ;
	List.iter ( fun bar -> 
	  let s = String.join " " (List.map html_of_chord bar) in
	  pf "<td class=\"grille\">%s</td>" s ;
	)  line ;
	pf "</tr>\n"  ;
    ) g ;
    pf "</table>\n"  ;
    pf "</div>\n" ;
) ;;

let write_lyrics fout l = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let (nbcols,l) = l in
  pf "<div class=\"lyrics%d\">\n" nbcols ;
  pf "%s" "<p>\n" ;
  List.iter ( fun line ->
    if line="\\" then (
      pf "%s" "</p>\n<p>\n" 
    )  else (
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "<span class=\"remarque\">\\1</span>" line in
      let reg = Str.regexp "\\[\\([^;]*\\);\\([^]]*\\)]" in
      let line = Str.global_replace reg "<span class=\"chord\">\\1</span><span class=\"chord-position\">\\2</span>" line in
	pf "%s<br/>\n" line
    )
  ) l ;
  pf "%s" "</p>\n" ;
  pf "%s" "</div>\n" ; 
) 

let write_tab  (pf : ('a, unit, string, unit) format4 -> 'a) l = (
  pf "%s" "<div class=\"tablature\">\n<pre>" ;
  List.iter ( fun line ->
    if line="\\" then (
      pf "%s" "</p>\n<p class=\"tablature\">\n"
    )  else (
      let reg = Str.regexp "{\\(.*\\)}" in
      let line = Str.global_replace reg "<span class=\"remarque\">\\1</span>" line in
	pf "%s\n" line
    )
  ) l ;
  pf "%s" "</p>\n" ;
  pf "%s" "</pre></div>\n" ; 
) 

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


(*
let read_song filename : document = (
  let fin = open_in filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      match line with
      | "\\titre" -> r ((Titre (read_string_until_empty_line fin))::acc)
      | "\\auteur" -> r ((Auteur (read_string_until_empty_line fin))::acc)
      | "\\grille" -> r ((Grille (read_array_until_empty_line fin))::acc)
      | "\\lyrics" -> r ((Lyrics (read_array_until_empty_line fin))::acc)
      | "\\tab" -> r ((Tab (read_array_until_empty_line fin))::acc)
      | "\\mp3" -> r ((Mp3 (read_string_until_empty_line fin))::acc)
      | "" -> r acc
      | s -> r ((Normal s)::acc)
    with
    | End_of_file -> (
      close_in fin ;
      List.rev acc
    )
  in
  let data = r []   in
  data
) ;;
*)


let write_song fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let () = printf "write song %s\n" song.Song.titre in
    
  let _ = pf  "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\" media=\"screen\"/>
<link href=\"song-print.css\" type=\"text/css\" rel=\"stylesheet\" media=\"print\"/>
<title>%s</title>
</head>
<body>
" song.Song.titre  in


    pf "<a href=\"index.html#%s\">\n" song.Song.id ;
    pf "<div class=\"titre-auteur\">\n" ;
    pf "<div class=\"titre\">%s</div>\n" song.Song.titre ;
    pf "<div class=\"auteur\">%s</div>\n" song.Song.auteur ;
    pf "</div>\n" ;
    pf "</a>\n" ;


    List.iter ( fun c ->
      match c with 
	| Normal s -> pf "<span class=\"remarque\">%s</span><br/>" s
	| Titre _ 
	| Auteur _ -> ()
	| Grille (name,g) -> write_grille ~transpose:song.Song.transpose fout name (g:Accord.t list list list)
	| Lyrics l -> write_lyrics fout l
	| Mp3 l -> write_mp3 pf l
	| Tab l -> write_tab pf l
	| Accords l -> write_accords fout l
	| Transpose i -> pf "<span class=\"remarque\">Transpose de %d demi-tons</span><br/>" i
	| PageBreak -> pf "<div class=\"pagebreak\"></div>\n"
    ) song.Song.data
    ;
    fprintf fout "</body></html>" ;
)
