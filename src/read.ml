open ExtList
open ExtString
open Printf

open Datamodel

let (//) = Filename.concat

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

(*
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
*)

let _ = 
  try
    let () = Printexc.record_backtrace true in

    let opt = OptParse.OptParser.make () in
    let opt_showdeps = 
      let o = OptParse.StdOpt.store_false () in
      let () = OptParse.OptParser.add opt ~long_name:"show-deps" ~help:"show deps" o  in
	o
    in
    let opt_prefix = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"prefix" ~help:"installation prefix" o in
	o 
    in
    let opt_song = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"song" ~help:"song" o in
	o 
    in
    let opt_book = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"book" ~help:"book" o in
	o 
    in
    let opt_tmpdir = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"tmp-dir" ~help:"tmp dir" o in
	o 
    in

    let _ = OptParse.OptParser.parse_argv opt in

    let showdeps = OptParse.Opt.get opt_showdeps in
    let song = OptParse.Opt.opt opt_song in
    let book = OptParse.Opt.opt opt_book in
    let prefix = OptParse.Opt.opt opt_prefix in
    let tmpdir = OptParse.Opt.opt opt_tmpdir in

    let () = match showdeps,song,book,prefix,tmpdir with
      | true,Some song,None,None,None -> show_deps_song song
      | true,None,Some book,None,None -> show_deps_book book
      | false,Some filename,None,Some prefix,Some tmpdir -> Song.write (Song.read filename) prefix tmpdir
      | false,None,Some filename,Some prefix,Some tmpdir -> Book.write (Book.read filename) prefix tmpdir
      | _ -> failwith "bad args combination"
    in

      exit 0
  with
  | e -> (
    printf "xxxx %s\n" (Printexc.to_string e) ; 
    Printexc.print_backtrace stdout ;
    exit 1 
  )
