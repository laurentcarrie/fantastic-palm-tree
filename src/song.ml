module D = Datamodel
open ExtList
open Printf
open ExtString
open Read_util



let check_bar song = (
  ()
)


let read ~filename = (
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
		  let lignes = List.map ( fun l -> { D.Grille.bars = Read_util.Grille.bars_of_string l} ) l  in
		  let g = { D.Grille.titre=arg;lignes=lignes } in
		    r ((D.Grille g)::acc)
		)
	      | "\\tab" -> (
		  let l = read_array_until_empty_line fin in
		  let tab = Read_util.tablature_of_string_list arg l in
		    r ((D.Tab tab)::acc)
		)
	      | "\\lyrics" -> (
		  let l =  { D.Lyrics.nb_cols=1;title=arg;data=(read_array_until_empty_line fin) } in r((D.Lyrics l)::acc)
		)
	      | "\\lyrics2" -> (
		  let l =  { D.Lyrics.nb_cols=2;title=arg;data=(read_array_until_empty_line fin) } in r((D.Lyrics l)::acc)
		)
	      | "\\lyrics3" -> (
		  let l =  { D.Lyrics.nb_cols=3;title=arg;data=(read_array_until_empty_line fin) } in r((D.Lyrics l)::acc)
		)
	      | "\\mp3" -> r ((D.Mp3 (read_string_until_empty_line fin))::acc)
	      | "\\transpose" -> r ((D.Transpose (int_of_string (read_string_until_empty_line fin))::acc))
	      | "\\nb-croches" 
	      | "\\nb_croches" 
		-> r ((D.Nb_croches (int_of_string (read_string_until_empty_line fin))::acc))
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
  let nb_croches = List.fold_left ( fun acc d -> match d with | D.Nb_croches i -> i | _ -> acc ) 8 data in
  let () = assert(title<>"") in
  let song = {D.Song.filename=filename;titre=title;auteur=auteur;data=data;transpose=transpose;nb_croches=nb_croches} in
  let () = check_bar song in

    song
) ;;

let msgstring_of_bool b msg = if b then msg else ""

let print_deps ~song ~top_build_dir = (
  let name = (Filename.chop_extension song.D.Song.filename) in
  let (_,deps) = List.fold_left ( fun (count,acc) data ->
    match data with 
      | D.Tab tab -> (
	  Write_mp_tab.write_mp song name tab count ;
	  (*
	    let acc = sprintf "%s\n%s.tex: %s-%d.mps" acc name name count in
	    let acc = sprintf "%s\n%s.tex: %s-%d.mp" acc name name count in
	    let acc = sprintf "%s\n%s.tex: %s-%d.1" acc name name count in
	  *)
	  count+1,acc
	)
      | _ -> (count,acc)
  ) (0,"") song.D.Song.data in
  let () = 
    let fout = open_out "write model" (song.D.Song.filename ^ ".model") in
    let () = fprintf fout "transpose : %d\n" song.D.Song.transpose in
    let () = fprintf fout "filename : '%s'\n" song.D.Song.filename in
    let () = fprintf fout "titre : '%s'\n" song.D.Song.titre in
    let () = fprintf fout "auteur : '%s'\n" song.D.Song.auteur in
    let () = fprintf fout "nb_croches : '%d'\n" song.D.Song.nb_croches in
    let () = fprintf fout "data : \n" in
    let () = List.iter ( fun d ->
      match d with
	| D.Normal s -> fprintf fout " normal : %s\n" s
	| D.Titre s -> fprintf fout " titre : %s\n" s
	| D.Auteur s -> fprintf fout " auteur : %s\n" s
	| D.Mp3 s -> fprintf fout " mp3 : %s\n" s
	| D.Transpose s -> fprintf fout " transpose : %d\n" s
	| D.Nb_croches s -> fprintf fout " nb_croches : %d\n" s
	| D.PageBreak -> fprintf fout " page break : \n" 
	| D.Grille g  -> (
	    fprintf fout " grille %s\n" g.D.Grille.titre ;
	    List.iter ( fun l -> 
	      fprintf fout "    ligne \n" ;
	      List.iter ( fun b -> 
		fprintf fout "      bar : " ;
		List.iter ( fun c ->
		  (match c.D.Accord.position with
		    | None -> fprintf fout "        pas de position" ;
		    | Some p -> fprintf fout "        %d " p
		  ) ;
		  match c.D.Accord.chord with
		    | None -> ()
		    | Some c -> (
			fprintf fout "[%c %s %s %s %s %s %s] "       
			  c.D.Accord.note
			  (match c.D.Accord.alteration with | D.Accord.None -> "" | D.Accord.Flat -> "flat" | D.Accord.Sharp -> "sharp")
			  (msgstring_of_bool c.D.Accord.minor "minor")
			  (msgstring_of_bool c.D.Accord.minor7 "minor7")
			  (msgstring_of_bool c.D.Accord.major7 "major7")
			  (msgstring_of_bool c.D.Accord.diminue "diminue")
			  (msgstring_of_bool c.D.Accord.sus4 "sus4")
		      )
		) b.D.Grille.chords ;
		fprintf fout "\n" ;
	      ) l.D.Grille.bars
	    ) g.D.Grille.lignes
	      
	  )
	| D.Lyrics l -> (
	    fprintf fout "  lyrics %d cols, title '%s'\n" l.D.Lyrics.nb_cols l.D.Lyrics.title 
	  )
	| D.Tab t -> (
	    fprintf fout "  tab \n" ;
	    fprintf fout "    title : %s\n" t.D.Tablature.titre ;
	    List.iter ( fun l ->
	      fprintf fout "      ligne \n" ;
	      List.iter ( fun b ->
		fprintf fout "        bar \n" ;
		List.iter ( fun p ->
		  fprintf fout "          paquet " ;
		  List.iter ( fun n ->
		    fprintf fout "[ %d %d ]" n.D.Tablature.corde n.D.Tablature.frette
		  ) p.D.Tablature.notes ;
		  fprintf fout "\n" ;
		) b.D.Tablature.paquets
	      ) l.D.Tablature.bars
	    ) t.D.Tablature.lines
	  )
	| D.Accords l -> (
	    fprintf fout "  accords \n" ;
	    List.iter ( fun s -> fprintf fout "%s\n" s ) l
	  )

    ) song.D.Song.data in
    let () = close_out fout in
      ()
  in
    
    eprintf "%s\n" deps  ; flush stderr 
      (* printf "%s\n" deps  ; flush stdout  *)
      
)


let write ~song ~prefix = (
  let f = Filename.basename song.D.Song.filename in
  let f = Filename.chop_suffix f ".song" in
  let fout = open_out "write_pdf_song" ( ( f ^ ".tex" )) in
    Write_pdf_song.write_song fout song
)
