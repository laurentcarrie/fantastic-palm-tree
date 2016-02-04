open ExtList
open ExtString
open Printf

let (//) = Filename.concat

type context =
    | Normal of string
    | Titre of string
    | Auteur of string
    | Grille of string list
    | Lyrics of string list
    | Mp3 of string

type document = context list 


let print_title (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<h1>%s</h1>" title
) ;;

let print_mp3 (pf : ('a, unit, string, unit) format4 -> 'a) title = (
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
    let reg = Str.regexp "{\\(.*\\)}" in
    let line = Str.global_replace reg "<span class=\"remarque\">\\1</span>" line in
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
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\" media=\"screen\"/>
<link href=\"song-print.css\" type=\"text/css\" rel=\"stylesheet\" media=\"print\"/>
<title>%s</title>
</head>
<body>
" title  in


    pf "<a href=\"index.html\">\n" ;
    pf "<div class=\"titre\">%s</div>\n" title ;
    pf "<div class=\"auteur\">%s</div>\n" auteur ;
    pf "</a>\n" ;

    List.iter ( fun c ->
      match c with 
      | Normal s -> pf "%s<br/>" s
      | Titre _ 
      | Auteur _ -> ()
      | Grille g -> print_grille pf (g:string list)
      | Lyrics l -> print_lyrics pf l
      | Mp3 l -> print_mp3 pf l
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

  let letters : char list  = 
    let c0 = Char.code 'A' in
      Array.to_list ( Array.init 26 ( fun i -> Char.chr (i+c0) ) )
  in

  let write_index_letters () = ( 
    let fout = open_out (Sys.argv.(2) // "index-letters.html") in
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
      List.iter ( fun l ->
	let songs = List.filter ( fun (_,titre,_) ->
	  let i = String.get (String.uppercase titre) 0 in
	    l = i
	) songs in
	let count = List.length songs in
	pf "<a href=\"./index-letter-%c.html\">%c <span class=\"letter-count\">(%d chansons)</span></a><br/>\n" l l count
      ) letters ;
      close_out fout
  )
  in


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
    let () = pf "%s" "<a href=\"./index-letters.html\">lettres</a>" in
    let songs = List.sort ~cmp:(fun (_,titre1,auteur1) (_,titre2,auteur2) ->
      match String.compare titre1 titre2 with
      | 0 -> String.compare auteur1 auteur2
      | n -> n
    ) songs in

    List.iteri ( fun index (html,titre,auteur) ->
      let html = String.slice ~first:(String.length Sys.argv.(2)) html in
      let d = index mod 2 in
      pf "<div class=\"index-entry-%d\"><a href=\".%s\"><span class=\"index-titre-%d\">%s</span> <span class=\"index-auteur-%d\">(%s)</span></a></div>\n" 
	d html d titre d auteur ;
    ) songs ;
      close_out fout
  in (* write index *)

  let write_index_one_letter songs l = (
    let fout = open_out (Sys.argv.(2) // (sprintf "index-letter-%c.html" l)) in
    let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    let _ = pf "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>index des chansons, lettre %c</title>
</head>
<body>
"  l in
    let () = pf "Index lettre %c</br>" l in
    let songs = List.filter ( fun (_,titre,_) ->
      let i = String.get (String.uppercase titre) 0 in
	l = i
    ) songs in
    List.iteri ( fun index (html,titre,auteur) ->
      let html = String.slice ~first:(String.length Sys.argv.(2)) html in
      let d = index mod 2 in
      pf "<div class=\"index-entry-%d\"><a href=\".%s\"><span class=\"index-titre-%d\">%s</span> <span class=\"index-auteur-%d\">(%s)</span></a></div>\n" 
	d html d titre d auteur ;
    ) songs ;
      close_out fout

    
  ) in

    write_index_letters () ;
    write_index songs ;
    List.iter ( fun l -> write_index_one_letter songs l ) letters
