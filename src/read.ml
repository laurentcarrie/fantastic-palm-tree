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
  let fin = open_in "read song" filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      if String.starts_with line "#" then
	r acc 
      else (
	let (word,arg) = try String.split line " " with | ExtString.Invalid_string -> line,"" in
	match word with
	| "\\titre" -> r ((Titre (read_string_until_empty_line fin))::acc)
	| "\\auteur" -> r ((Auteur (read_string_until_empty_line fin))::acc)
	| "\\grille" -> (
	    let (l:string list) = read_array_until_empty_line fin in
	    let lignes = List.map Read_util.barlist_of_string l in
	    let g = { Grille.titre=arg;lignes=lignes } in
	      r ((Grille g)::acc)
	  )
	| "\\tab" -> (
	    let l = read_array_until_empty_line fin in
	    let lines = Read_util.tab_of_string_list l in
	    let tab = { Tablature.titre=arg;lines=lines } in
	      r ((Tab tab)::acc)
	  )
	| "\\lyrics" -> r ((Lyrics (1,arg,(read_array_until_empty_line fin))::acc))
	| "\\lyrics2" -> r ((Lyrics (2,arg,(read_array_until_empty_line fin))::acc))
	| "\\lyrics3" -> r ((Lyrics (3,arg,(read_array_until_empty_line fin))::acc))
	| "\\mp3" -> r ((Mp3 (read_string_until_empty_line fin))::acc)
	| "\\transpose" -> r ((Transpose (int_of_string (read_string_until_empty_line fin))::acc))
	| "\\pagebreak" -> r ((PageBreak :: acc))
	| "\\chords"
	| "\\accords" -> 
	  r ((Accords ((read_array_until_empty_line fin)))::acc)
	| "" -> r acc
	| s -> r ((Normal s)::acc)
      )
    with
    | End_of_file -> (
      close_in fin ;
      List.rev acc
    )
  in
  let data = r []   in
  data
) ;;


let manage_book filename book_id = (
  let fin = open_in "manage book" filename in
  let print_index=false in
  let rec r print_index acc =
    try
      let line = String.strip ( input_line fin ) in
	match line with
	  | "\\print_index" -> r true acc
	  | line -> r print_index (line::acc)
    with
      | End_of_file -> close_in fin ; (print_index,List.rev acc)
  in
  let (print_index,songs) = r print_index [] in 
  let titre = Filename.basename filename in
  let () = assert (titre <> "" ) in
    {Book.filename=filename;titre;auteur="book";id=book_id;songs=songs;print_index=print_index}
)


let manage_song filename fileout_latex song_id = (
  try
    let data = read_song filename in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "???" data in
    let auteur = List.fold_left ( fun acc d -> match d with | Auteur s -> s | _ -> acc ) "???" data in
    let transpose = List.fold_left ( fun acc d -> match d with | Transpose h -> h | _ -> acc ) 0 data in
    let () = assert(title<>"") in
    let song = {Song.filename=filename;titre=title;auteur=auteur;id=song_id;data=data;transpose=transpose;} in
    let () =
      let fout = open_out "manage song latex" (fileout_latex) in
      let () = Write_pdf_song.write_song fout song in
      close_out fout ;
    in
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
	      let fileout_latex = ((Filename.chop_suffix e ".song") ^ ".tex") in
	      let song = manage_song (dirname//e) fileout_latex (sprintf "song-%d" count) in
		(song::songs,books)
	    with
	      | e -> printf "%s\n" (Printexc.to_string e) ; (songs,books)
	  )
	| "book" -> (
	    try 
	      let count = List.length books in
	      let id = sprintf "book-%d" count in
	      let book = manage_book (dirname//e) id in
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
  try
    let () = Printexc.record_backtrace true in () ;
      assert(Array.length Sys.argv > 2) ;

      let prefix = Sys.argv.(2) in
	
  let (songs,books) = walk ([],[]) Sys.argv.(1) prefix in

    let books = 
      let all = { Book.filename="all.book" ; titre = "all" ; auteur = "yyy" ; id = "xxx" ; 
		  songs = List.sort ~cmp:Helpers.uppercase_compare (List.map ( fun b -> b.Song.titre ) songs) ;
		  print_index=true ;} in
      all::books 
    in


      List.iter ( fun b -> printf "book : %s (%d songs)\n" b.Book.filename (List.length b.Book.songs) ;  Write_pdf_song.write_book b songs ) books ;

      exit 0
  with
  | e -> (
    printf "xxxx %s\n" (Printexc.to_string e) ; 
    Printexc.print_backtrace stdout ;
    exit 1 
  )
