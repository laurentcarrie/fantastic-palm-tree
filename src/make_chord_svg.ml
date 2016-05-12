open Printf
open ExtList

let (//) = Filename.concat

let main () =

  let opt = OptParse.OptParser.make () in
  let opt_show_targets = 
    let o = OptParse.StdOpt.store_false () in
    let () = OptParse.OptParser.add opt ~long_name:"show-targets" ~help:"show targets" o  in
    o
  in
  let opt_prefix = 
    let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
    let () = OptParse.OptParser.add opt ~long_name:"prefix" ~help:"installation prefix" o in
    o 
  in
  let _ = OptParse.OptParser.parse_argv opt in


  let prefix = OptParse.Opt.get opt_prefix in
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

  let chords = List.sort ~cmp:( fun (_,c1) (_,c2) -> String.compare c1.Chord.filename c2.Chord.filename ) chords in

  let () = if OptParse.Opt.get opt_show_targets then (
    let fout = open_out "png_digest.txt" in
    List.iter (    fun (i,c) -> 
      let svg_filename = "tmp" // c.Chord.filename ^ ".svg" in
      let png_filename = prefix // "png" // (c.Chord.filename ^ ".png") in
      let () = Chord.write_svg svg_filename c in
      let command = sprintf "inkscape --export-png %s --file %s" png_filename svg_filename in
      let () = printf "%s\n" command ; flush stdout ; in
      let ret = Unix.system command in
      (match ret with
      | Unix.WEXITED 0 -> Unix.unlink svg_filename ; let d = Digest.file png_filename in fprintf fout "%s %s\n" (Filename.basename png_filename) (Digest.to_hex d) ;
      | _ -> (let msg = sprintf "%s failed" command in failwith msg)
      ) ;
    ) chords ;
    close_out fout
  )
  else (
    List.iter ( fun (i,c) ->
      printf "%s.png " (prefix//"png"//c.Chord.filename)
    ) chords 
  )
  in
  ()
    
let _ = 
  let () = main () in
  let () = printf "svg files written.\n" ; flush stdout ; in
  ()
    

