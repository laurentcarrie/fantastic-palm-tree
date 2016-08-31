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
    let opt_makedict = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"make-dictionary" ~help:"make dictionary" o  in
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
    let opt_tmpdir = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"tmp-dir" ~help:"tmp dir" o in
	o 
    in
    let opt_srcdir = 
      let o = OptParse.Opt.value_option "" None (fun a->a) ( fun e _ -> Printexc.to_string e) in
      let () = OptParse.OptParser.add opt ~long_name:"srcdir" ~help:"srcdir" o in
	o 
    in

    let args = OptParse.OptParser.parse_argv opt in

    let makedict = OptParse.Opt.opt opt_makedict in
    let showdeps = OptParse.Opt.get opt_showdeps in
    let song = OptParse.Opt.opt opt_song in
    let book = OptParse.Opt.opt opt_book in
    let prefix = OptParse.Opt.opt opt_prefix in
    let tmpdir = OptParse.Opt.opt opt_tmpdir in
    let srcdir = OptParse.Opt.opt opt_srcdir in

    let () = match makedict,showdeps,song,book,prefix,tmpdir,srcdir with
      | Some filename,false,None,None,None,None,None -> make_dictionary filename args
      | None,true,Some filename,None,None,None,None -> Song.print_deps (Song.read ~filename:filename)
      | None,true,None,Some filename,None,None,Some srcdir -> Book.print_deps (Book.read ~filename ~srcdir )
      | None,false,Some filename,None,Some prefix,Some tmpdir,None -> Song.write (Song.read ~filename) prefix tmpdir
      | None,false,None,Some filename,Some prefix,Some tmpdir,Some srcdir -> Book.write ~book:(Book.read ~filename ~srcdir) ~tmpdir 
      | _ -> failwith "bad args combination"
    in

      exit 0
  with
  | e -> (
    Printexc.print_backtrace stdout ;
    exit 1 
  )
