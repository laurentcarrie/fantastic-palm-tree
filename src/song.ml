module D = Datamodel
open ExtList
open Printf
open ExtString
open Read_util


(*
let manage_song filename fileout_latex song_id = (
  try
    let data = read_song filename in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "???" data in
    let auteur = List.fold_left ( fun acc d -> match d with | Auteur s -> s | _ -> acc ) "???" data in
    let transpose = List.fold_left ( fun acc d -> match d with | Transpose h -> h | _ -> acc ) 0 data in
    let () = assert(title<>"") in
    let song = {Datamodel.Song.filename=filename;titre=title;auteur=auteur;id=song_id;data=data;transpose=transpose;} in
    let () =
      let fout = open_out "manage song latex" (fileout_latex) in
      let () = Write_pdf_song.write_song fout song in
      close_out fout ;
    in
    song
  with
    | e -> failwith (sprintf "ERREUR : %s\n" (Printexc.to_string e) )
) ;;
*)



let read filename  = (
  let fin = open_in "read song" filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      if String.starts_with line "#" then
	r acc 
      else (
	let (word,arg) = try String.split line " " with | ExtString.Invalid_string -> line,"" in
	match word with
	| "\\titre" -> r ((D.Titre (read_string_until_empty_line fin))::acc)
	| "\\auteur" -> r ((D.Auteur (read_string_until_empty_line fin))::acc)
	| "\\grille" -> (
	    let (l:string list) = read_array_until_empty_line fin in
	    let lignes = List.map Read_util.barlist_of_string l in
	    let g = { D.Grille.titre=arg;lignes=lignes } in
	      r ((D.Grille g)::acc)
	  )
	| "\\tab" -> (
	    let l = read_array_until_empty_line fin in
	    let lines = Read_util.tab_of_string_list l in
	    let tab = { D.Tablature.titre=arg;lines=lines } in
	      r ((D.Tab tab)::acc)
	  )
	| "\\lyrics" -> r ((D.Lyrics (1,arg,(read_array_until_empty_line fin))::acc))
	| "\\lyrics2" -> r ((D.Lyrics (2,arg,(read_array_until_empty_line fin))::acc))
	| "\\lyrics3" -> r ((D.Lyrics (3,arg,(read_array_until_empty_line fin))::acc))
	| "\\mp3" -> r ((D.Mp3 (read_string_until_empty_line fin))::acc)
	| "\\transpose" -> r ((D.Transpose (int_of_string (read_string_until_empty_line fin))::acc))
	| "\\pagebreak" -> r ((D.PageBreak :: acc))
	| "\\chords"
	| "\\accords" -> 
	  r ((D.Accords ((read_array_until_empty_line fin)))::acc)
	| "" -> r acc
	| s -> r ((D.Normal s)::acc)
      )
    with
    | End_of_file -> (
      close_in fin ;
      List.rev acc
    )
  in
  let data = r []   in
  let title = List.fold_left ( fun acc d -> match d with | D.Titre s -> s | _ -> acc ) "???" data in
  let auteur = List.fold_left ( fun acc d -> match d with | D.Auteur s -> s | _ -> acc ) "???" data in
  let transpose = List.fold_left ( fun acc d -> match d with | D.Transpose h -> h | _ -> acc ) 0 data in
  let () = assert(title<>"") in
  let song = {D.Song.filename=filename;titre=title;auteur=auteur;data=data;transpose=transpose;} in

      song
) ;;


let print_deps song = (
  printf "\n" ; flush stdout
  
)


let write song prefix tmpdir = (
  let f = Filename.basename song.D.Song.filename in
  let f = Filename.chop_suffix f ".song" in
  let fout = open_out "write_pdf_song" ( tmpdir // ( f ^ ".tex" )) in
    Write_pdf_song.write_song fout song
)