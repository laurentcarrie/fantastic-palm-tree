open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat


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

let read_song filename : document = (
  let fin = open_in filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      match line with
      | "\\titre" -> r ((Titre (read_string_until_empty_line fin))::acc)
      | "\\auteur" -> r ((Auteur (read_string_until_empty_line fin))::acc)
      | "\\grille" -> r ((Grille (read_array_until_empty_line fin))::acc)
      | "\\tab" -> r ((Tab (read_array_until_empty_line fin))::acc)
      | "\\lyrics" -> r ((Lyrics (read_array_until_empty_line fin))::acc)
      | "\\mp3" -> r ((Mp3 (read_string_until_empty_line fin))::acc)
      | "\\transpose" -> r ((Transpose (int_of_string (read_string_until_empty_line fin))::acc))
      | "\\pagebreak" -> r ((PageBreak :: acc))
      | "\\chords"
      | "\\accords" -> 
	  r ((Accords ((read_array_until_empty_line fin)))::acc)
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
    {Book.filename=fileout;titre=filename;auteur="book";id=book_id;songs=songs}
)


let manage_song filename fileout song_id = (
  try
    let data = read_song filename in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "???" data in
    let auteur = List.fold_left ( fun acc d -> match d with | Auteur s -> s | _ -> acc ) "???" data in
    let transpose = List.fold_left ( fun acc d -> match d with | Transpose h -> h | _ -> acc ) 0 data in
    let song = {Song.filename=fileout;titre=title;auteur=auteur;id=song_id;data=data;transpose=transpose;} in

    let () = printf "open %s\n" fileout ; flush stdout ; in
    let fout = open_out fileout in
    let () = Write_song.write_song fout song in
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
	      (*let fileout = dirout // (sprintf "song-%d.html" count) in*)
	      let fileout = dirout // (sprintf "%s.html" (Filename.(Filename.basename (Filename.chop_suffix e ".song")))) in
	      let song = manage_song (dirname//e) fileout (sprintf "song-%d" count) in
		(song::songs,books)
	    with
	      | e -> printf "%s\n" (Printexc.to_string e) ; (songs,books)
	  )
	| "book" -> (
	    try 
	      let count = List.length books in
	      (*let fileout = dirout // (sprintf "book-%d.html" count) in*)
	      let fileout = dirout // (sprintf "book-%s.html" (Filename.chop_suffix e ".book")) in
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
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\" media=\"screen\"/>
<link href=\"song-print.css\" type=\"text/css\" rel=\"stylesheet\" media=\"print\"/>
<title>index des livres</title>
</head>
<body>
"   in
      pf "<ul>\n" ;
      let songs = List.map ( fun b ->
	try 
	  let song = List.find ( fun s -> s.Song.titre = b ) songs  in
	    (b,Some song)
	with
	  | Not_found -> (b,None)
      ) book.Book.songs in

      let () = pf "<ul>\n" in
      let () = 
	List.iter ( fun (b,song) -> 
	  match song with
	    | Some _ -> ()
	    | None -> 
		pf "<li>not found : '%s'</li>\n" b 
	) songs in
      let () = pf "</ul>\n" in
	
      let () =
	List.iteri ( fun index (b,song) ->
	  match song with
	    | None -> ()
	    | Some song -> (
		Write_song.write_song fout song ;
		pf "<p style=\"page-break-after:always;\"></p>\n" ;
	      )
	) songs in
	pf "
</body>
</html>
" ;
      close_out fout
  ) in




    write_index_letters () ;
    Write_index.write songs ;
    let books = 
      let all = { Book.filename=Sys.argv.(2)//"all.html" ; titre = "all" ; auteur = "" ; id = "xxx" ; 
		  songs = List.sort ~cmp:Helpers.uppercase_compare (List.map ( fun b -> b.Song.titre ) songs) } in
	all::books 
    in
      write_index_books books ;
      List.iter ( fun b -> write_book b songs ) books ;
      List.iter ( fun l -> write_index_one_letter songs l ) letters
