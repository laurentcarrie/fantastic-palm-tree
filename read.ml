open ExtString
open Printf

type context =
    | Normal
    | Grille of string
(*
module String = struct
  include String
  let split s sep = 
    try
      split s sep
    with
      | Invalid_string -> failwith("invalid string : '" ^ s ^ "'")
end
*)
let read filename = 
  let fin = open_in filename in
  let rec r acc context =
    try
      let line = input_line fin in
      let line = 
	try 
	  let (command,line2) = String.split line " " in
	    match command with
	      | "\\title" 
	      | "\\titre" -> sprintf "<h1>%s</h1>" line2
	      | _ -> line
	with 
	  | Invalid_string -> line
	in
	r (line::acc) context
    with
      | End_of_file -> (
	  close_in fin ;
	  List.rev acc
	)
  in

  let data = r [] Normal in
    data


let _ =
  try
    let data = read Sys.argv.(1) in
    let en_tete = "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>hello</title>
</head>
<body>
" in
    let pied_de_page = "</body></html>" in
    let data = en_tete ^ (String.join "\n" data) ^ pied_de_page in
      Std.output_file ~filename:Sys.argv.(2) ~text:data
  with
    | e -> printf "ERREUR : %s\n" (Printexc.to_string e)


