open ExtString
open Printf


type context =
    | Normal
    | Titre of string
    | Grille of string list

type document = context list 


let print_title (pf : ('a, unit, string, unit) format4 -> 'a) title = (
  pf "<h1>%s</h1>" title
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

let print_grille  (pf : ('a, unit, string, unit) format4 -> 'a) (g:string list) = (
  pf "%s" "\n<table>\n" ;
  List.iter ( fun line ->
    pf "%s" "<tr>"  ;
    let a = String.nsplit line ":" in
    List.iter ( fun a -> pf "<td>%s</td>" a ) a ;
    pf "%s" "</tr>\n"  ;
  ) g ;
  pf "%s" "</table>\n" 
) ;;

let read filename : document = 
  let fin = open_in filename in
  let rec r acc  = 
    try
      let line = String.strip (input_line fin) in
      match line with
      | "\\titre" -> r ((Titre (read_string_until_empty_line fin))::acc)
      | "\\grille" -> r ((Grille (read_array_until_empty_line fin))::acc)
      | s -> failwith ("bad string : '" ^ s ^ "'")
    with
    | End_of_file -> (
      close_in fin ;
      List.rev acc
    )
  in
  let data = r []  in
  data
    

let _ =
  try
    let data = read Sys.argv.(1) in
    let title = List.fold_left ( fun acc d -> match d with | Titre s -> s | _ -> acc ) "" data in
    let fout = open_out (Sys.argv.(2)) in
    let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in

    let _ = pf  "<!DOCTYPE html>
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"UTF-8\">
<link href=\"song.css\" type=\"text/css\" rel=\"stylesheet\"/>
<title>%s</title>
</head>
<body>
" title  in

    List.iter ( fun c ->
      match c with 
      | Normal -> ()
      | Titre s -> print_title pf s
      | Grille g -> print_grille pf (g:string list)
    ) data
    ;
    fprintf fout "</body></html>" ;
    close_out fout
  with
    | e -> printf "ERREUR : %s\n" (Printexc.to_string e)


