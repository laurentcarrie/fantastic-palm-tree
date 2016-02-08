open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat

let write_title (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<h1>%s</h1>" title
) ;;

let write_mp3 (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<div class=\"mp3\"><a href=%s>./%s</a></div>" title title
) ;;

let read_array_until_empty_line fin = (
  let rec r acc =
    try
      let line = input_line fin in
      let line = String.strip line in
      if line = "" then ( List.rev acc) else ( r (line::acc))
    with
    | End_of_file -> (List.rev acc)
  in
  r []
) ;;

let read_string_until_empty_line fin = (
  let a = read_array_until_empty_line fin in
  String.join "\n" a
) ;;

let html_of_chord c = (
  List.fold_left ( fun c (sub,by) ->
    let reg = Str.regexp (Str.quote sub) in
    Str.global_replace reg by c
  ) c  [
    "#","&#x266f;" ;
    "b","&#x266d;" ;
    "m","<sub>m</sub>" ;
  ]
)
let write_grille  (pf : ('a, unit, string, unit) format4 -> 'a) (g:string list) = (
  pf "%s" "\n<table>\n" ;
  List.iter ( fun line ->
    pf "%s" "<tr>"  ;
    let a = String.nsplit line ":" in
    List.iter ( fun a -> pf "<td class=\"grille\">%s</td>" (html_of_chord a) ) a ;
    pf "%s" "</tr>\n"  ;
  ) g ;
  pf "%s" "</table>\n" 
) ;;

let write_lyrics  (pf : ('a, unit, string, unit) format4 -> 'a) l = (
  pf "%s""<div class=\"lyrics\">\n" ;
  let l = List.map ( fun line -> if line="\\" then "" else line) l in
  List.iter ( fun line ->
    let reg = Str.regexp "{\\(.*\\)}" in
    let line = Str.global_replace reg "<span class=\"remarque\">\\1</span>" line in
    pf "%s<br/>\n" line
  ) l ;
  pf "%s" "</div>\n" ; 
) 

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



let write fout song = (

  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    
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
    pf "<div class=\"titre\">%s</div>\n" song.Song.titre ;
    pf "<div class=\"auteur\">%s</div>\n" song.Song.auteur ;
    pf "</a>\n" ;

    List.iter ( fun c ->
      match c with 
      | Normal s -> pf "%s<br/>" s
      | Titre _ 
      | Auteur _ -> ()
      | Grille g -> write_grille pf (g:string list)
      | Lyrics l -> write_lyrics pf l
      | Mp3 l -> write_mp3 pf l
    ) song.Song.data
    ;
    fprintf fout "</body></html>" ;
)
