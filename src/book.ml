open ExtList
open ExtString
open Printf
open Read_util

module D = Datamodel

let print_deps book = (
  printf "\n" ; flush stdout 
)


let read ~filename   = (
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
    {D.Book.filename=filename;titre;auteur="book";songs=songs;print_index=print_index}
)


let write ~book ~prefix ~tmpdir ~srcdir = (
  let songs = List.fold_left ( fun acc filename ->
    let (name,song) =
      try
	let s = Song.read (srcdir // filename) in
	  s.D.Song.titre , Some s
      with
	| _ -> (
	    printf "XXXXXXXXXXXXXXXXXXXXXXXXX '%s'\n" (srcdir//filename) ;
	    (srcdir//filename),None
	  )
    in
      (name,song)::acc
  ) [] book.D.Book.songs in
  let songs = List.rev songs in
    Write_pdf_song.write_book ~book ~songs ~tmpdir 
)
