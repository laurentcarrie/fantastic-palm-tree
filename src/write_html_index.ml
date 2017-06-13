open ExtList
open ExtString
open Printf
open Read_util
open Write_util


module D = Datamodel

let (//) = Filename.concat

let write ~top_src_dir = (
  let fout = open_out "html_index" ( "index.html" ) in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

  let all_books = Make_all.all_books ~top_src_dir in
  let all_books = List.map ( fun f -> Book.read ~filename:f ~top_src_dir ) all_books in

  let all_songs = Make_all.all_songs ~top_src_dir in
  let all_songs = List.map ( fun f -> Song.read ~filename:f ) all_songs in

  let by_auteur = List.fold_left ( fun acc song ->
    let auteur = String.uppercase_ascii song.D.Song.auteur in
    let a = try List.assoc auteur acc with | Not_found -> [] in
    let a = (auteur,(song::a)) in
    let acc = List.remove_assoc auteur acc in
      a::acc
  ) [] all_songs in

  let by_auteur = List.sort ~cmp:(fun (a1,_) (a2,_) -> String.compare a1 a2) by_auteur in
  let by_auteur = List.map ( fun (a,songs) -> 
    (a,List.sort ~cmp:(fun s1 s2 -> String.compare s1.D.Song.titre s2.D.Song.titre) songs)
  ) by_auteur in

  let () = pf "%s" "
<html>
<head>
<meta charset=\"UTF-8\"/>
<title>songs index</title>
<body>
"
  in

  let () = pf "%s" "<h2><a href=\"pdf/book-all.pdf\">all</a></h2>\n" in
  let () = pf "%s" "<h2>books</h2>\n<ul>" in
  let () = List.iter ( fun book ->
      let f = Filename.chop_suffix (Filename.basename book.D.Book.filename) ".book" in
      let f = Str.replace_first (Str.regexp (Str.quote top_src_dir//"")) "" f in
	pf "<li><a href=\"pdf/books/book-%s.pdf\">%s</a></li>"
	  f book.D.Book.titre
  ) all_books in
  let () = pf "%s" "</ul><h2>songs</h2>\n" in

  let () = List.iter ( fun (auteur,songs) ->
    let () = pf "<h3>%s</h3>\n<ul>\n" auteur in
    let () = List.iter ( fun song ->
      let f = Filename.chop_suffix song.D.Song.filename ".song" in
      let f = Str.replace_first (Str.regexp (Str.quote top_src_dir//"")) "" f in
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

