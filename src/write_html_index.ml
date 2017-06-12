open ExtList
open ExtString
open Printf
open Read_util
open Write_util


module D = Datamodel

let (//) = Filename.concat

let write ~top_src_dir = (
  let book = Book.read ~filename:"all.book" ~top_src_dir in
  let fout = open_out "html_index" ( "index.html" ) in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let by_auteur = List.fold_left ( fun acc song ->
    match song with
      | D.Book.S song -> (
	  let auteur = String.uppercase_ascii song.D.Song.auteur in
	  let a = try List.assoc auteur acc with | Not_found -> [] in
	  let a = (auteur,(song::a)) in
	  let acc = List.remove_assoc auteur acc in
	    a::acc
	)
      | D.Book.NF _ -> acc
  ) [] book.D.Book.songs in

  let by_auteur = List.sort ~cmp:(fun (a1,_) (a2,_) -> String.compare a1 a2) by_auteur in

  let () = pf "%s" "
<html>
<head>
<meta charset=\"UTF-8\"/>
<title>songs index</title>
<body>
"
  in

  let () = List.iter ( fun (auteur,songs) ->
    let () = pf "<h3>%s</h3>\n<ul>\n" auteur in
    let () = List.iter ( fun song ->
      let f = Filename.chop_suffix song.D.Song.filename ".song" in
      let f = Str.replace_first (Str.regexp (Str.quote top_src_dir//"")) "" f in
(*
      let () = pf "top_src_dir %s\n" top_src_dir in
      let () = pf "prefix %s\n" prefix in
      let () = pf "dirname %s\n" song.D.Song.dirname in
      let () = pf "filename %s\n" song.D.Song.filename in
*)
      let () = pf "<li><a href=\"pdf/%s.pdf\">%s</a></li>\n"
	f song.D.Song.titre
      in 
	()
    ) songs in
    let () = pf "%s" "</ul>\n" in
      ()
  ) by_auteur in

  let () = pf "
</body>
</html>
" in
  let () = close_out fout in
  ()
)

