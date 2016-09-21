module D = Datamodel

open ExtList
open ExtString
open Printf


let tex_of_chord (c:D.Accord.t) = (
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
  (* let s = if subscript="" then s else s^"\\textsubscript{"^subscript^"}" in  *)
  let s = if subscript="" then s else s^"{"^subscript^"}" in  
  let s = if s="%" then "" else ""^s^"" in
  s
)

let mp_of_chord (c:D.Accord.t) : (string*string*string) = (
  let s = sprintf "%c" c.D.Accord.note in
  let fb = match c.D.Accord.alteration with
    | D.Accord.None -> ""
    | D.Accord.Flat -> "$\\flat$"
    | D.Accord.Sharp ->"$\\sharp$"
  in
  let s = s ^ fb in
  let upperscript = "" in
  let s = if c.D.Accord.diminue then s^"\\o" else s in
  let subscript = "" in
  let subscript = if c.D.Accord.minor then subscript^"m" else subscript in
  let subscript = if c.D.Accord.minor7 then subscript^"7" else subscript in
  let subscript = if c.D.Accord.major7 then subscript^"7M" else subscript in
  let subscript = if c.D.Accord.sus4 then subscript^"sus4" else subscript in
    s,subscript,upperscript
)
