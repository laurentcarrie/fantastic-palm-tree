open ExtList
open ExtString
open Printf
open Read_util

let (//) = Filename.concat

let make_dictionary filename songs = (
  let fout = open_out "make dict" filename in
    List.iter ( fun s ->
      fprintf fout "%s\n" s
    ) songs ;
    close_out fout
)


let _ = 
  try
    let () = Printexc.record_backtrace true in

    let opt = OptParse.OptParser.make () in
    let opt_showdeps = 
      let o = OptParse.StdOpt.store_true () in
      let () = OptParse.OptParser.add opt ~long_name:"show-deps" ~help:"show deps" o  in
	o
    in
    let opt_makeall = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"make-all" ~help:"make book all" o  in
	o
    in
    let opt_make_html_index = 
      let o = OptParse.StdOpt.store_true () in 
      let () = OptParse.OptParser.add opt ~long_name:"write-html-index" ~help:"make html index" o  in
	o
    in
    let opt_prefix = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"prefix" ~help:"installation prefix" o in
	o 
    in
    let opt_song = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"song" ~help:"song" o in
	o 
    in
    let opt_book = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"book" ~help:"book" o in
	o 
    in
    let opt_top_build_dir = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"top-build-dir" ~help:"top build dir" o in
	o 
    in
    let opt_top_src_dir = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"top-src-dir" ~help:"top src dir" o in
	o 
    in

    let _ = OptParse.OptParser.parse_argv opt in

    let makeall = OptParse.Opt.opt opt_makeall in
    let make_html_index = OptParse.Opt.get opt_make_html_index in
    let showdeps = OptParse.Opt.get opt_showdeps in
    let song = OptParse.Opt.opt opt_song in
    let book = OptParse.Opt.opt opt_book in
    let prefix = OptParse.Opt.opt opt_prefix in
    let top_build_dir = OptParse.Opt.opt opt_top_build_dir in
    let top_src_dir = OptParse.Opt.opt opt_top_src_dir in

    let () = if make_html_index then (
      let top_src_dir = Option.get top_src_dir in
	Write_html_index.write ~top_src_dir 
    ) else (
      let () = match makeall,showdeps,song,book,prefix,top_build_dir,top_src_dir with
	| Some filename,false,None,None,None,None,Some top_src_dir -> Make_all.make ~filename ~top_src_dir
	| None,true,Some filename,None,None,Some top_build_dir,None -> Song.print_deps ~song:(Song.read ~filename) ~top_build_dir
	| None,true,None,Some filename,None,Some top_build_dir,Some top_src_dir -> Book.print_deps ~book:(Book.read ~filename ~top_src_dir ) ~top_build_dir ~top_src_dir
	| None,false,Some filename,None,Some prefix,None,None -> Song.write (Song.read ~filename) prefix 
	| None,false,None,Some filename,Some prefix,Some top_build_dir,Some top_src_dir -> (
	    Book.write ~book:(Book.read ~filename ~top_src_dir) 
	  )
	| _ -> failwith "bad args combination"
      in
	()
    )
    in


      exit 0
  with
  | e -> (
      eprintf "Exception caught : %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr ;
      exit 1 
  )
