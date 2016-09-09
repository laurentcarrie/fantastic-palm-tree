open ExtList
open ExtString
open Printf
open Read_util

module D = Datamodel

let print_deps ~book ~top_src_dir ~top_build_dir = (
  let () = eprintf "top_build_dir : '%s'\n" top_build_dir in
  let () = eprintf "top_src_dir : '%s'\n" top_src_dir in
  let tex_name = (Filename.chop_extension book.D.Book.filename) ^ ".tex" in
  let deps = List.fold_left ( fun acc d ->
    match d with 
      (* | D.Book.NF s -> acc ^ " " ^ "Not found : " ^ s *)
      | D.Book.NF _ -> acc
      | D.Book.S song -> (
	  let song_tex =  (Filename.chop_extension song.D.Song.filename) in
	  (* let song_tex = Str.replace_first (Str.regexp (Str.quote (top_src_dir//""))) "" song_tex in *)
	  (* let song_tex =  top_build_dir // song_tex in *)
	  let s2 = Str.replace_first (Str.regexp (Str.quote (top_src_dir//""))) "" song_tex in 
	  let acc = sprintf "%s\nbook-%s : %s.tex\n" acc tex_name (top_build_dir // s2) in 
	  let acc = sprintf "%s\nbook-%s : %s.song\n" acc tex_name (top_build_dir // s2) in 
	  let song_tex = Filename.basename song_tex in
	    (* let () = Song.print_deps ~song ~top_build_dir in  *)
	  let (_,acc) = List.fold_left ( fun (count,acc) tab ->
	    let () = Write_mp_tab.write_mp song song_tex tab count in
	      (*
		let acc = sprintf "%s\nbook-%s : %s-%d.mps\n" acc tex_name song_tex count in	
		let acc = sprintf "%s\nbook-%s : %s-%d.1\n" acc tex_name song_tex count in
		let acc = sprintf "%s\nbook-%s : %s-%d.mp\n" acc tex_name song_tex count in
	      *)
	      (count+1,acc)
	  ) (0,acc) (D.Song.tabs_of_song song) in
	    acc
	)
  ) "" book.D.Book.songs in
    
    eprintf "XXXXXXXXXXXXXXXXXXXXX book-%s: %s\n" tex_name deps  ; flush stderr ;
    printf "book-%s: %s\n" tex_name deps  ; flush stdout 
)


let read ~filename ~top_src_dir  = (
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
      let s = Song.read (top_src_dir // filename) in
	eprintf "Ok path found : '%s'\n" (top_src_dir // filename) ;
	D.Book.S s
    with
      | _ -> (
	  eprintf "Path not found : '%s'\n" (top_src_dir // filename) ;
	  D.Book.NF  (filename)
	)
  )  songs in
    {D.Book.filename=filename;titre;auteur="book";songs=songs;print_index=print_index}
)


let write ~book = (
  Write_pdf_song.write_book ~book 
)
