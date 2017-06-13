open ExtList
open ExtString
open Printf
open Read_util

module D = Datamodel

let (//) = Filename.concat


let all_books ~top_src_dir =  (
    let rec r dirname acc =
      let paths = Array.to_list ( Sys.readdir dirname ) in
      let acc = List.fold_left ( fun acc e ->
	let full_e = dirname // e in
	  if Sys.is_directory full_e then
	    r full_e acc
	  else (
	    if Filename.check_suffix e ".book" then
	      full_e::acc
	    else
	      acc
	  )
      ) acc paths 
      in
	acc
    in
      r top_src_dir []
)

let all_songs ~top_src_dir =  (
    let rec r dirname acc =
      let paths = Array.to_list ( Sys.readdir dirname ) in
      let acc = List.fold_left ( fun acc e ->
	let full_e = dirname // e in
	  if Sys.is_directory full_e then
	    r full_e acc
	  else (
	    if Filename.check_suffix e ".song" then
	      full_e::acc
	    else
	      acc
	  )
      ) acc paths 
      in
	acc
    in
      r top_src_dir []
)

let make ~filename ~top_src_dir = (

  let songs =  all_songs ~top_src_dir in

  let songs = List.sort ~cmp:(fun s1 s2 -> String.compare (Filename.basename s1) (Filename.basename s2)) songs in

  let fout = open_out "all.song" filename in 
  let () = fprintf fout "\\print_index \n" in
  let () = List.iter ( fun s ->
    let s = Str.replace_first (Str.regexp (Str.quote (top_src_dir//""))) "" s in
      fprintf fout "%s\n" s
  ) songs in
  let () = close_out fout in
	       
  (* let () = Make_index.make ~songs in *)
	       
  let fout = open_out "all.model" (filename^".model") in 
  let () = fprintf fout "\\print_index \n" in
  let () = List.iter ( fun s ->
    let s = Str.replace_first (Str.regexp (Str.quote (top_src_dir//""))) "" s in
      fprintf fout "%s\n" s
  ) songs in
  let () = close_out fout in
    ()
	       
)
