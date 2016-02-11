open Printf
open ExtList

let chord_name c = 
  let s = Str.global_replace (Str.regexp (Str.quote "#")) "-sharp" c.Chord.name in
    s

let main () =

  let () = List.iter ( fun (i,c) ->
    let out_filename = chord_name c ^ ".svg" in
    let () = printf "write file '%s\n" out_filename ; flush stdout ; in
    let () = Chord.write_svg out_filename i (i+5) c in
      ()
  ) ( List.map ( fun i -> (i,Chord.e_form i)) (List.init 12 (fun i->i)) ) in
    ()

let _ = 
  main ()
    
