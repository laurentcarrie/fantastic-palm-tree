open ExtList
open ExtString
open Printf

let (//) = Filename.concat

let int_of_string s = 
  try
    int_of_string s
  with
    | _ -> failwith ("not a string : " ^ s)

type context =
    | Normal of string
    | Titre of string
    | Auteur of string
    | Grille of string list
    | Lyrics of string list
    | Tab of string list
    | Mp3 of string
    | Accords of string list
    | Transpose of int

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

