open Datamodel
open ExtList
open ExtString
open Printf

let (//) = Filename.concat

let open_out msg s = 
  (* eprintf "open_out (%s) %s\n" msg s; flush stdout ; *)
  open_out s

let open_in msg s = 
  (* eprintf "open_in (%s) %s\n" msg s ; flush stdout ; *)
  open_in s


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
    { Accord.note = note ; minor=minor ; alteration=alteration ; minor7=minor7 ; major7=major7 ; diminue=diminue ; sus4=sus4}
  in
  let bar_of_string s =
    List.map chord_of_string (String.nsplit s " ")
  in
  List.map bar_of_string (String.nsplit s ":")
) ;;

let note_of_string s : (int*Tablature.note) = (
  let s2 = String.nsplit s  " " in
  let l = List.map int_of_string s2 in
    match l with
      | position::duration::corde::frette::[] -> (
	  if corde<1 || corde>6 then (
	    let msg = sprintf "in %s, bad string : %d" s corde in failwith msg
	  ) ;
	  if frette<0 || frette>20 then (
	    let msg = sprintf "in %s, bad frette : %d" s frette in failwith msg
	  ) ;
	  position,{Tablature.duration=duration;corde=corde;frette=frette}
	)
      | _ -> let msg = sprintf "error for bar : '%s'" s in failwith msg
) ;;

let bar_of_string s : Tablature.bar = (
  let init = [] (*List.init 4 (fun i -> (2*i+1),[])*) in
  let s2 = String.nsplit s ";" in
    List.fold_left ( fun acc s ->
      let (pos,note) = note_of_string s in
      let old = try List.assoc pos acc with | Not_found -> [] in
      let acc = List.remove_assoc pos acc in
	(pos,(note::old))::acc
    )  init s2
) ;;

let tab_of_string_list lines : Tablature.line list = (
  let line_of_string_list line : Tablature.line =
    let a = String.nsplit line "|" in
    let a = List.filter ( fun s -> s <> "") a in
    let bars = List.map bar_of_string a in
      bars
  in
    List.map line_of_string_list lines
) ;;



let read_array_until_empty_line fin = (
  let rec r acc =
    try
      let line = input_line fin in
      let line = String.strip line in
      if line = "" then ( List.rev acc) else ( r (line::acc))
    with
    | End_of_file -> (List.rev acc)
  in
  r []
) ;;


let read_string_until_empty_line fin = (
  let a = read_array_until_empty_line fin in
  String.join "\n" a
) ;;

