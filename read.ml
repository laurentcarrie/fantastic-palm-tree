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

module Song = struct
  type t = {
    filename:string ;
    titre:string ;
    auteur:string ;
    id:string ;
    data:context list ;
  }
end

module Book = struct
  type t = {
    filename:string ;
    titre:string ;
    auteur:string ;
    id:string ;
    songs:string list ;
  }
end

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


let manage_book filename fileout book_id = (
  let () = printf "open book '%s'\n" filename ; flush stdout ; in
  let fin = open_in filename in
  let rec r acc =
    try
      let line = String.strip ( input_line fin ) in
	r (line::acc)
    with
      | End_of_file -> close_in fin ; List.rev acc
  in
  let songs = r [] in 
    {Book.filename=fileout;titre=book_id;auteur="book";id=book_id;songs=songs}
)

let print_song fout song = (

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
      | Grille g -> print_grille pf (g:string list)
      | Lyrics l -> print_lyrics pf l
      | Mp3 l -> print_mp3 pf l
    ) song.Song.data
    ;
    fprintf fout "</body></html>" ;
)

let manage_song filename fileout song_id = (
  try
    let data = read_song filename in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "???" data in
    let auteur = List.fold_left ( fun acc d -> match d with | Auteur s -> s | _ -> acc ) "???" data in
    let song = {Song.filename=fileout;titre=title;auteur=auteur;id=song_id;data=data} in

    let () = printf "open %s\n" fileout ; flush stdout ; in
    let fout = open_out fileout in
    let () = print_song fout song in
    close_out fout ;
      song
  with
    | e -> failwith (sprintf "ERREUR : %s\n" (Printexc.to_string e) )
) ;;


let rec walk (songs,books) dirname dirout = (
  printf "enter directory %s\n" dirname ; flush stdout ;
  let entries = Sys.readdir dirname in
  let entries = Array.to_list entries in
  let (songs,books) = List.fold_left ( fun (songs,books) e ->
    if Sys.is_directory (dirname//e) then walk (songs,books) (dirname // e) dirout
    else (
      let suffix = List.last (String.nsplit e ".") in
      match suffix with
	| "song" -> (
	    try 
	      let count = List.length songs in
	      let fileout = dirout // (sprintf "song-%d.html" count) in
	      let song = manage_song (dirname//e) fileout (sprintf "song-%d" count) in
		(song::songs,books)
	    with
	      | e -> printf "%s\n" (Printexc.to_string e) ; (songs,books)
	  )
	| "book" -> (
	    try 
	      let count = List.length books in
	      let fileout = dirout // (sprintf "book-%d.html" count) in
	      let id = sprintf "book-%d" count in
	      let book = manage_book (dirname//e) fileout id in
		(songs,book::books)
	    with
	      | e -> printf "%s\n" (Printexc.to_string e) ; (songs,books)
	  )
	| _ ->	songs,books
    )
  ) (songs,books) entries in
    songs,books
)
  

let _ = 
  assert(Array.length Sys.argv > 2) ;

  let (songs,books) = walk ([],[]) Sys.argv.(1) Sys.argv.(2) in

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
	let songs = List.filter ( fun t ->
	  let i = 
	    if t.Song.titre="" then ' ' else (String.get (String.uppercase t.Song.titre) 0) in
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
    let () = pf "%s" "<div class=\"index-letters\"><a href=\"./index-letters.html\">lettres</a></div>" in
    let () = pf "%s" "<div class=\"index-books\"><a href=\"./index-books.html\">livres</a></div>" in
    let songs = List.sort ~cmp:(fun t1 t2 ->
      match String.compare t1.Song.titre t2.Song.titre with
      | 0 -> String.compare t1.Song.auteur t2.Song.auteur
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
    let () = pf "<a href=\"./index.html#letter-%c\">Index lettre %c</a></br>" l l in
    let songs = List.filter ( fun t ->
      let i = String.get (String.uppercase t.Song.titre) 0 in
	l = i
    ) songs in
    let _ = List.fold_left ( fun (initiale,index) t ->
      let html = String.slice ~first:(String.length Sys.argv.(2)) t.Song.filename in
      let current = String.get (String.uppercase t.Song.titre) 0 in
	let initiale = if current<>initiale then (
	  pf "<div class=\"nouvelle-initiale\"><a href=\"./index.html#letter-%c\">--- %c ---</a></div>\n" current current ;
	  current
	)
	  else (
	    initiale 
	  )
	in
	let d = index mod 2 in
	  pf "<div class=\"index-entry-%d\"><a href=\".%s\"><span class=\"index-titre-%d\">%s</span> <span class=\"index-auteur-%d\">(%s)</span></a></div>\n" 
	    d html d t.Song.titre d t.Song.auteur ;
	  (initiale,index+1)
    ) (' ',0) songs in
      pf "
</body>
</html>
" ;
      close_out fout
  ) in


  let write_index_books books = (
    let fout = open_out (Sys.argv.(2) // "index-books.html") in
    let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    let () = pf "%s" "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>index des livres</title>
</head>
<body>
"   in
      List.iteri ( fun index b ->
	let html = String.slice ~first:(String.length Sys.argv.(2)) b.Book.filename in
	let d = index mod 2 in
	  pf "<div class=\"index-book-%d\"><a href=\".%s\">%s</a></div>\n" d html b.Book.titre
      ) books ;
      pf "
</body>
</html>
" ;
      close_out fout
  ) in


  let write_book book songs = (
    let fout = open_out book.Book.filename in
    let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
    let () = pf "%s" "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>index des livres</title>
</head>
<body>
"   in
      pf "this is book</br>\n" ;
      pf "<ul>\n" ;
      List.iter ( fun b -> pf "<li>%s</li>\n" b ) book.Book.songs ;
      pf "</ul>\n" ;
      List.iteri ( fun index b ->
	pf "%s</br>" b ;
	let () = try
		   let song = List.find ( fun s -> s.Song.titre = b ) songs in
		   print_song fout song ;
		   pf "<p style=\"page-break-after:always;\"></p>\n" ;
	  with
	    | Not_found -> printf "song %s not found\n" b
	in
	  ()
      ) book.Book.songs ;
      pf "
</body>
</html>
" ;
      close_out fout
  ) in




    write_index_letters () ;
    write_index songs ;
    write_index_books books ;
    List.iter ( fun b -> write_book b songs ) books ;
    List.iter ( fun l -> write_index_one_letter songs l ) letters
