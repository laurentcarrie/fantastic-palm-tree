open ExtList
open ExtString
open Printf

let (//) = Filename.concat

let int_of_string s = 
  try
    int_of_string s
  with
    | _ -> failwith ("not a string : " ^ s)


module Accord = struct
  type alteration = | None | Flat | Sharp
  type t = {
    note : char ;
    alteration : alteration ;
    minor : bool ;
    minor7 : bool ;
    major7 : bool ;
    diminue : bool ;
    sus4 : bool ;
  }
end

module Grille = struct
  type case = Accord.t list
  type ligne = case list
  type t = {
    titre : string ;
    lignes : ligne list ;
  }
end

module Tablature = struct
  type note = {
    duration:int ;
    frette:int ;
    corde:int ;
  }
  type bar = (int*note list) list 
  type line = bar list
  type t = {
    titre : string ;
    lines : line list ;
  }
end

type context =
| Normal of string
| Titre of string
| Auteur of string
| Grille of Grille.t
| Lyrics of (int*string*string list)
| Tab of Tablature.t
| Mp3 of string
| Accords of string list
| Transpose of int
| PageBreak
    
type document = context list 

module Song = struct
  type t = {
    transpose:int ;
    filename:string ;
    titre:string ;
    auteur:string ;
    data:context list ;
  }

  let tabs_of_song song = (
    let tabs = List.fold_left ( fun acc d ->
      match d with
	| Tab t -> t::acc
	| _ -> acc
    ) [] song.data in
      List.rev tabs
  )


end

module Book = struct
  type s =
      | NF of string (* not found *)
      | S of Song.t
  type t = {
    filename:string ;
    titre:string ;
    auteur:string ;
    songs:s list ;
    print_index:bool ;
  }
end


