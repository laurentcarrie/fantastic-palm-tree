open Printf

let main () =
  let out_filename = Sys.argv.(1) in
  let () = printf "Writing to %s\n" out_filename in
  let () = Chord.write_svg out_filename Chord.c_form in
    ()

let _ = 
  main ()
    
