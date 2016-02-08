open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat


let write songs =  
  let fout = open_out (Sys.argv.(2) // "index.html") in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let _ = pf "%s"  "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>index des chansons</title>
</head>
<body>
"  in
  let () = pf "%s" "<div class=\"index-letters\"><a href=\"./index-letters.html\">lettres</a></div>" in
  let () = pf "%s" "<div class=\"index-books\"><a href=\"./index-books.html\">livres</a></div>" in
  let songs = List.sort ~cmp:(fun t1 t2 ->
    match Helpers.uppercase_compare t1.Song.titre t2.Song.titre with
      | 0 -> Helpers.uppercase_compare t1.Song.auteur t2.Song.auteur
      | n -> n
  ) songs in

  let _ = List.fold_left ( fun (initiale,index) t ->
    let html = String.slice ~first:(String.length Sys.argv.(2)) t.Song.filename in
    let current = String.get (String.uppercase t.Song.titre) 0 in
    let initiale = if current<>initiale then (
      pf "<a name=\"letter-%c\"/><div class=\"nouvelle-initiale\"><a href=\"index-letter-%c.html\">--- %c ---</a></div>\n" current current current ;
      current
    )
      else (
	initiale 
      )
    in
    let d = index mod 2 in 
      pf "<a name=\"%s\"/><div class=\"index-entry-%d\"><a href=\".%s\"><span class=\"index-titre-%d\">%s</span> <span class=\"index-auteur-%d\">(%s)</span></a></div>\n" 
	t.Song.id
	d html d t.Song.titre d t.Song.auteur ;
      initiale,index+1
  ) (' ',0) songs in
    pf "
</body>
</html>
" ;
    close_out fout
