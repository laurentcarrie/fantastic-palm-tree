#include <string>
#include <fstream>
#include <sstream>
#include <vector>
#include <numeric>
#include <functional>

#include "read_util.h"

void check_note(char a) {
    switch (a) {
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
        return ;
    default :
    {
        std::ostringstream oss ;
        oss << "not a note : " << a << std::endl ;
        throw std::runtime_error(oss.str()) ;
    }
    }
}


/*
let chord_of_string s : Accord.c = (
let a = String.explode s in
let (note,a) = match a with
  | [] -> failwith "empty bar"
| note::a -> note,a  in
let () = check_note note in
let (alteration,a) = match a with
  | [] -> Accord.None,[]
  | 'b'::a -> Accord.Flat,a
  | '#'::a -> Accord.Sharp,a
  | _ -> Accord.None,a
in
let (sus4,a) = match a with
  | [] -> false,[]
  | 's'::'u'::'s'::'4'::a -> true,a
  | _ -> false,a
in
let (diminue,a) = match a with
  | [] -> false,[]
  | 'd'::'i'::'m'::a -> true,a
  | _ -> false,a
in
let (minor,a) = match a with
  | [] -> false,[]
  | 'm'::a -> true,a
  | _ -> false,a
in
let (minor7,major7,a) = match a with
  | [] -> false,false,[]
  | '7'::'M'::a -> false,true,a
  | '7'::a -> true,false,a
  | _ -> false,false,a
in
  { Accord.note = note ; minor=minor ; alteration=alteration ; minor7=minor7 ; major7=major7 ; diminue=diminue ; sus4=sus4 }
)

let position_and_silence_or_chord_of_string s : Accord.t = (
let a = String.nsplit s "," in
let a = List.map String.strip a in
  match a with
    | [] -> failwith "erreur de format"
    | "s"::[] -> { Accord.position=None ; chord = None }
    | n::"s"::[] -> { Accord.position=Some(int_of_string n) ; chord = None }
    | n::hd::[] -> { Accord.position=Some (int_of_string n) ; chord=Some (chord_of_string hd) }
    | hd::[] -> { Accord.position=None ; chord=Some (chord_of_string hd) }
    | _ -> let msg = sprintf "not managed : '%s'" s in failwith msg
)

module Grille = struct
open Datamodel.Grille

let bars_of_string (s:string) = (
  let bar_of_string s =
    { chords = List.map position_and_silence_or_chord_of_string (String.nsplit s " ") }
in
    List.map bar_of_string (String.nsplit s ":")
) ;;

end

let note_of_string s : (Tablature.note) = (
let s2 = String.nsplit s  " " in
  (
    let l = List.map int_of_string s2 in
match l with
  | corde::frette::[] -> (
      if corde<1 || corde>6 then (
	let msg = sprintf "in %s, bad string : %d" s corde in failwith msg
      ) ;
      if frette<0 || frette>20 then (
	    let msg = sprintf "in %s, bad frette : %d" s frette in failwith msg
      ) ;
      {Tablature.corde=corde;frette=frette}
    )
  | _ -> let msg = sprintf "error for bar : '%s'" s in failwith msg
  )
) ;;

let paquet_of_string s : Tablature.paquet = (
let s2 = String.nsplit s "," in
let (s2,(chord:Accord.c option)) =
  let c = List.hd s2 in
  let c = String.strip c in
    if String.starts_with c "[" && String.ends_with c "]" then (
let l = String.length c in
let c = String.slice c ~first:1 ~last:(l-1) in
  (List.tl s2,Some (chord_of_string c))
    ) else
(s2,None)
in
let s2 = List.map String.strip s2 in
let position = int_of_string(List.hd s2) in
let notes = List.map note_of_string (List.tl s2) in
  { Tablature.notes=notes ; chord={Accord.position=Some position;chord=chord }}
) ;;

let bar_of_string s = (
let s2 = String.nsplit s ";" in
  { Datamodel.Tablature.paquets = List.map paquet_of_string s2 }
) ;;

let tablature_of_string_list titre lines = (
let line_of_string_list line : Tablature.line =
  let a = String.nsplit line "|" in
  let a = List.filter ( fun s -> s <> "") a in
  let bars = { Datamodel.Tablature.bars = List.map bar_of_string a } in
    bars
in
  { Tablature.titre=titre ; lines = List.map line_of_string_list lines }
) ;;

*/

void strip_string(std::string& s) {
    if (s=="") return ;
    if (s[0]==' ') {
        s.erase(s.begin()) ;
        strip_string(s) ;
    }
    return ;
}

std::vector<std::string> read_array_until_empty_line(std::ifstream& fin) {
    std::vector<std::string> ret ;
    for (;;)
    {
        std::string line  ;
        getline(fin,line) ;
        strip_string(line) ;
        if (line == "") {
            break ;
        }
        ret.insert(ret.begin(),line);
    }
}




std::string read_string_until_empty_line(std::ifstream& fin) {
    std::vector<std::string> a = read_array_until_empty_line(fin) ;
    std::string ret = std::accumulate(a.begin(),a.end(),std::string(""),[](std::string acc,std::string inc) {
        return acc + "\n" + inc ;
    }) ;
    return ret ;
}


