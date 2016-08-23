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

type context =
| Normal of string
| Titre of string
| Auteur of string
| Grille of Grille.t
| Lyrics of (int*string*string list)
| Tab of string list
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
    id:string ;
    data:context list ;
  }
end

module Book = struct
  type t = {
    filename:string ;
    titre:string ;
    auteur:string ;
    id:string ;
    songs:string list ;
  }
end



let open_out msg s = 
  printf "open_out (%s) %s\n" msg s; flush stdout ;
  open_out s

let open_in msg s = 
  printf "open_in (%s) %s\n" msg s ; flush stdout ;
  open_in s
