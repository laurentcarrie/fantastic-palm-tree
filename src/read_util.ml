open Datamodel
open ExtList
open ExtString

let barlist_of_string (s:string) : Accord.t list list = (
  let chord_of_string s = 
    let s = String.strip s in
    let a = String.explode s in 
    let (note,a) = match a with
      | [] -> failwith "empty bar"
      | note::a -> note,a  in
    let (alteration,a) = match a with
      | [] -> Accord.None,[]
      | 'b'::a -> Accord.Flat,a
      | '#'::a -> Accord.Sharp,a
      | _ -> Accord.None,a
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
    { Accord.note = note ; minor=minor ; alteration=alteration ; minor7=minor7 ; major7=major7 }
  in
  let bar_of_string s =
    List.map chord_of_string (String.nsplit s " ")
  in
  List.map bar_of_string (String.nsplit s ":")
) ;;
