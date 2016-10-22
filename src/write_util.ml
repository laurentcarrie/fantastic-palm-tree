module D = Datamodel

open ExtList
open ExtString
open Printf

let tex_of_chord (c:D.Accord.t) = (
  match c.D.Accord.chord with
    | Some c -> 
	(
	  let s = sprintf "%c" c.D.Accord.note in
	  let s = match c.D.Accord.alteration with
	    | D.Accord.None -> s
	    | D.Accord.Flat -> s ^ "$\\flat$"
	    | D.Accord.Sharp -> s ^ "$\\sharp$"
	  in
	  let s = if c.D.Accord.diminue then s^"ø" else s in
	  let subscript = "" in
	  let subscript = if c.D.Accord.minor then subscript^"m" else subscript in
	  let subscript = if c.D.Accord.minor7 then subscript^"7" else subscript in
	  let subscript = if c.D.Accord.major7 then subscript^"7M" else subscript in
	  let subscript = if c.D.Accord.sus4 then subscript^"sus4" else subscript in
	  let s = if subscript="" then s else s^"\\textsubscript{\\small{"^subscript^"}}" in  
	  (* let s = if subscript="" then s else s^"{"^subscript^"}" in   *)
	  let s = if s="%" then "" else ""^s^"" in
	    s
	)
    | None -> (
	"silence"
      )
)


let mp_of_chord (c:D.Accord.c) : (string*string*string) = (
  let s = sprintf "%c" c.D.Accord.note in
  let fb = match c.D.Accord.alteration with
    | D.Accord.None -> ""
    | D.Accord.Flat -> "\\flatsharpfont{$\\flat$}"
    | D.Accord.Sharp ->"\\flatsharpfont{$\\sharp$}"
  in
  let s = s ^ fb in
  let upperscript = "" in
  let s = if c.D.Accord.diminue then s^"\\o" else s in
  let subscript = "" in
  let subscript = if c.D.Accord.minor then subscript^"m" else subscript in
  let upperscript = if c.D.Accord.minor7 then upperscript^"7" else upperscript in
  let subscript = if c.D.Accord.major7 then subscript^"$\\bigtriangleup$" else subscript in
  let upperscript = if c.D.Accord.sus4 then upperscript^"4" else upperscript in
    s,subscript,upperscript
)
