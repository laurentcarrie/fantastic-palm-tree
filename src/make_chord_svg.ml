open Printf
open ExtList

let (//) = Filename.concat

let main () =
  
  let l_0_11 = List.init 12 (fun i->i) in

  let functions = [
    Chord.e_form ;
    Chord.e7_form ;
    Chord.em_form ;
    Chord.em7_form ;
    Chord.e7M_form ; 

    Chord.a_form ;
    Chord.a7_form ;
    Chord.am_form ;
    Chord.am7_form ;
    Chord.a7M_form ; 

    Chord.c_form ;
    Chord.c7_form ;
    Chord.cm_form ;
    Chord.cm7_form ;
    Chord.c7M_form ; 

    Chord.g_form ;
    Chord.g7_form ;
    Chord.gm_form ;
    Chord.gm7_form ;
    Chord.g7M_form ; 

  ] in

  let chords = List.fold_left ( 
    fun acc f -> List.fold_left ( fun acc i -> (i,(f i))::acc ) acc l_0_11
  ) [] functions in
    
  let () = List.iter ( fun (i,c) ->
    let out_filename = "tmp" // c.Chord.filename ^ ".svg" in
    let () = printf "write file '%s\n" out_filename ; flush stdout ; in
    let () = Chord.write_svg out_filename c in
      ()
  ) chords in
    ()

let _ = 
  let () = main () in
  let () = printf "svg files written.\n" ; flush stdout ; in
    ()
    
