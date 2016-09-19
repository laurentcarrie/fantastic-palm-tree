open ExtList
open ExtString
open Printf
open Read_util

module D = Datamodel

let (//) = Filename.concat

let write_bar fout bar = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "
%% write bar
x1 := x0 + 2*ux + %d*ux ;
draw(x0,base_line) -- (x0,base_line+5*1uy) ;
" (* (List.length bar) *) 8  in
    
  let () = List.iter ( fun i ->
pf "
xii := x0 + %d*ux ;
pickup pencircle scaled 0.02bp ;
%%draw(xii,base_line) -- (xii,base_line+5*1uy) ;
pickup pencircle scaled 0.15bp ;
" i
  ) (List.init 4 ( fun i -> 2*i+1 ))
  in

  let _ = List.fold_left( fun pos p ->
    let notes = p.D.Tablature.notes in
    pf "
%% position %d, %d notes
xi := x0+%d*ux ;
pickup pencircle scaled 0.02bp ;
%%draw(xi,base_line) -- (xi,base_line+5*1uy) ;
pickup pencircle scaled 0.15bp ;
" pos (List.length notes) pos ;
      List.iter ( fun n ->
      pf "
  ynote := base_line+(6-%d)*uy ;
  label(\"%d\" infont defaultfont scaled 0.6,(xi,ynote)) ;
%%  draw(xi,ynote-uy) -- (xi,base_line-uy) ;
" n.D.Tablature.corde  n.D.Tablature.frette ;
      ) notes  ;

      let (lowest_note: int ) = 
	match notes with
	  | [] -> 6
	  | hd::tl -> (
	      List.fold_left ( fun acc (n:D.Tablature.note) ->
		if acc > n.D.Tablature.corde then acc else n.D.Tablature.corde
	      ) hd.D.Tablature.corde tl 
	    )
      in

      let print_tail c = (
      match p.D.Tablature.duration with
	| 1 ->
	    (*  double croche *)
      pf "
  ynote := base_line+(6-%d)*uy ;
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;
  draw(xi,base_line-1*uy) -- (xi+0.5*ux,base_line-0.8*uy) ;
  draw(xi,base_line-0.6*uy) -- (xi+0.5*ux,base_line-0.4*uy) ;
" c
	| 2 ->
	    (*  croche *)
      pf "
  ynote := base_line+(6-%d)*uy ;
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;
  draw(xi,base_line-1*uy) -- (xi+0.5*ux,base_line-0.8*uy) ;
" c
	| 4 ->
	    (* noire *)
      pf "
  ynote := base_line+(6-%d)*uy ;
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;
  fill fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor black ;
" c
	| 8 ->
	    (* blanche *)
      pf "
  ynote := base_line+(6-%d)*uy ;
  draw(xi,ynote-0.5*uy) -- (xi,base_line-1*uy) ;
  fill fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor white ;
  draw fullcircle scaled unote shifted (xi,base_line-1*uy) withcolor black ;
" c
	| _ -> ()

    ) in
    let () = print_tail lowest_note in
      pos + 1
  ) 1 bar in

    let () = pf "%s" "
for i=0 upto 5 :
   draw (x0,base_line+i*1uy) -- (x1,base_line+i*1uy) ;
  endfor
x0:=x1 ;
" in
    ()
)

let write_bar_line fout line = (
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
  x0:=0 ;

"  in
  let () = List.iter ( fun b -> write_bar fout b ) line in

  let () = pf "%s" "
draw(x1,base_line) -- (x1,base_line+5*1uy) ;
base_line := base_line + 6*ux ;
" in
    ()
)

let  write_mp song name tab count  = (
  let filename = sprintf  "%s-%d" (Filename.basename name) count in
  let fout = open_out "mp" (filename ^ ".mp") in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
beginfig(1) ;
  uy=0.2cm ;
  ux=0.25cm ;
  unote=0.1cm ;
  b=10*u ;
  base_line=0*u ;
  line_offset=10*u ;
  pickup pencircle scaled 0.15bp ;
  "  
in 

  let () = List.iter ( fun l -> write_bar_line fout l ) tab.D.Tablature.lines in

  
  let () = pf "
endfig ;
bye
" in
  let () = close_out fout 
  in
  let command = sprintf "mpost %s-%d.mp > /dev/null " (Filename.basename name) count in
  let ret = Unix.system command in
  let () = match ret with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "mpost failed"
  in
  let target = sprintf "%s-%d.1" (Filename.basename name) count in
  let () = if Sys.file_exists target then (
    let target2 =  sprintf "%s-%d.mps" (Filename.basename name) count  in
      Sys.rename target target2
  )
    else
      failwith "mpost failed, cannot find target file"
  in
    ()
)
