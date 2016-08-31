open ExtList
open ExtString
open Printf
open Read_util

module D = Datamodel

let print_deps book = (
  let deps = List.fold_left ( fun acc d ->
    match d with 
      (* | D.Book.NF s -> acc ^ " " ^ "Not found : " ^ s *)
      | D.Book.NF _ -> acc
      | D.Book.S song -> acc ^ " " ^ song.D.Song.filename
  ) "" book.D.Book.songs in
    
  let s = (Filename.chop_extension book.D.Book.filename) ^ ".tex" in
    eprintf "book-%s: %s\n" s deps  ; flush stderr ;
    printf "book-%s: %s\n" s deps  ; flush stdout 
)


let read ~filename ~srcdir  = (
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
  let songs = List.map ( fun  filename ->
    try
      let s = Song.read (srcdir // filename) in
	D.Book.S s
    with
      | _ -> (
	D.Book.NF  (filename)
	)
  )  songs in
    {D.Book.filename=filename;titre;auteur="book";songs=songs;print_index=print_index}
)


let write ~book  ~tmpdir  = (
  Write_pdf_song.write_book ~book ~tmpdir 
)
