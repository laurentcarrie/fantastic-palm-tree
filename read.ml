open ExtString
open Printf

let (//) = Filename.concat

type context =
    | Normal of string
    | Titre of string
    | Auteur of string
    | Grille of string list
    | Lyrics of string list

type document = context list 


let print_title (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<h1>%s</h1>" title
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
let print_grille  (pf : ('a, unit, string, unit) format4 -> 'a) (g:string list) = (
  pf "%s" "\n<table>\n" ;
  List.iter ( fun line ->
    pf "%s" "<tr>"  ;
    let a = String.nsplit line ":" in
    List.iter ( fun a -> pf "<td class=\"grille\">%s</td>" (html_of_chord a) ) a ;
    pf "%s" "</tr>\n"  ;
  ) g ;
  pf "%s" "</table>\n" 
) ;;

let print_lyrics  (pf : ('a, unit, string, unit) format4 -> 'a) l = (
  pf "%s""<div class=\"lyrics\">\n" ;
  let l = List.map ( fun line -> if line="\\" then "" else line) l in
  List.iter ( fun line ->
    pf "%s<br/>\n" line
  ) l ;
  pf "%s" "</div>\n" ; 
) 

let read filename : document = (
  let fin = open_in filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      match line with
      | "\\titre" -> r ((Titre (read_string_until_empty_line fin))::acc)
      | "\\auteur" -> r ((Auteur (read_string_until_empty_line fin))::acc)
      | "\\grille" -> r ((Grille (read_array_until_empty_line fin))::acc)
      | "\\lyrics" -> r ((Lyrics (read_array_until_empty_line fin))::acc)
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

let manage filename fileout = (
  try
    let data = read filename in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "???" data in
    let auteur = List.fold_left ( fun acc d -> match d with | Auteur s -> s | _ -> acc ) "???" data in
    let () = printf "open %s\n" fileout ; flush stdout ; in
    let fout = open_out fileout in
    let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

    let _ = pf  "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>%s</title>
</head>
<body>
<a href=\"index.html\">index</a>
" title  in

    pf "<div class=\"titre\">%s</div>\n" title ;
    pf "<div class=\"auteur\">%s</div>\n" auteur ;

    List.iter ( fun c ->
      match c with 
      | Normal s -> pf "%s<br/>" s
      | Titre _ 
      | Auteur _ -> ()
      | Grille g -> print_grille pf (g:string list)
      | Lyrics l -> print_lyrics pf l
    ) data
    ;
    fprintf fout "</body></html>" ;
    close_out fout ;
    fileout,title,auteur
  with
    | e -> failwith (sprintf "ERREUR : %s\n" (Printexc.to_string e) )
) ;;


let rec walk (count,outdata) dirname dirout = (
  printf "enter directory %s\n" dirname ; flush stdout ;
  let entries = Sys.readdir dirname in
  let entries = Array.to_list entries in
  let (count,outdata) = List.fold_left ( fun (count,outdata) e ->
    if Sys.is_directory (dirname//e) then walk (count,outdata) (dirname // e) dirout
    else (
      if Filename.check_suffix (dirname//e) ".song" then 
	try 
	  let fileout = dirout // (sprintf "song-%d.html" count) in
	  let info = manage (dirname//e) fileout  in
	  count+1,info::outdata
	with
	| e -> printf "%s\n" (Printexc.to_string e) ; count+1,outdata
      else
	count,outdata
    )
  ) (count,outdata) entries in
  count,outdata
)
  

let _ = 
  assert(Array.length Sys.argv > 2) ;

  let (_,songs) = walk (0,[]) Sys.argv.(1) Sys.argv.(2) in

  let write_index songs = 
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
    List.iter ( fun (html,titre,auteur) ->
      let html = String.slice ~first:(String.length Sys.argv.(2)) html in
      pf "<div class=\"index-entry\"><a href=\".%s\"><span class=\"index-titre\">%s</span> <span class=\"index-auteur\">%s</span></a></div>\n" html titre auteur ;
    ) songs
  in

  write_index songs
