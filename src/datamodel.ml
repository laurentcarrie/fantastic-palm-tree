open ExtList
open ExtString
open Printf

let (//) = Filename.concat

type context =
    | Normal of string
    | Titre of string
    | Auteur of string
    | Grille of string list
    | Lyrics of string list
    | Mp3 of string

type document = context list 

module Song = struct
  type t = {
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

