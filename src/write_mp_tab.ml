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
x1 := x0 + 2*u + %d*u ;
draw(x0,base_line) -- (x0,base_line+5*1u) ;
" (* (List.length bar) *) 8  in
    
  let () = List.iter ( fun i ->
pf "
xii := x0 + %d*u ;
pickup pencircle scaled 0.02bp ;
%%draw(xii,base_line) -- (xii,base_line+5*1u) ;
pickup pencircle scaled 0.15bp ;
" i
  ) (List.init 4 ( fun i -> 2*i+1 ))
  in

  let _ = List.fold_left( fun pos p ->
    let notes = p.D.Tablature.notes in
    pf "
%% position %d, %d notes
xi := x0+%d*u ;
pickup pencircle scaled 0.02bp ;
%%draw(xi,base_line) -- (xi,base_line+5*1u) ;
pickup pencircle scaled 0.15bp ;
" pos (List.length notes) pos ;
      List.iter ( fun n ->
      pf "
  ynote := base_line+(6-%d)*u ;
  label(\"%d\" infont defaultfont scaled 0.6,(xi,ynote)) ;
%%  draw(xi,ynote-u) -- (xi,base_line-u) ;
" n.D.Tablature.corde  n.D.Tablature.frette ;
      ) notes  ;

      let (lowest_note: int option) = List.fold_left ( fun acc (n:D.Tablature.note) ->
	match acc with
	  | None -> Some n.D.Tablature.corde
          | Some c -> Some (if c > n.D.Tablature.corde then c else n.D.Tablature.corde)
      ) None notes in

    let print_tail c = (
      match p.D.Tablature.duration with
	| 1 ->
	    (*  croche *)
      pf "
  ynote := base_line+(6-%d)*u ;
  draw(xi,ynote-0.5*u) -- (xi,base_line-1*u) ;
  draw(xi,base_line-1*u) -- (xi+0.5*u,base_line-1*u) ;
" c
	| 2 ->
	    (* noire *)
      pf "
  ynote := base_line+(6-%d)*u ;
  draw(xi,ynote-0.5*u) -- (xi,base_line-1*u) ;
" c
	| 4 ->
      pf "
  ynote := base_line+(6-%d)*u ;
  draw(xi,ynote-0.5*u) -- (xi,base_line-1*u) ;
" c
	| _ -> ()

    ) in
    let () = Option.may print_tail lowest_note in
      pos + 1
  ) 1 bar in

    let () = pf "%s" "
for i=0 upto 5 :
   draw (x0,base_line+i*1u) -- (x1,base_line+i*1u) ;
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
draw(x1,base_line) -- (x1,base_line+5*1u) ;
base_line := base_line + 6*u ;
" in
    ()
)

let  write_mp song name tab count  = (
  let filename = sprintf  "%s-%d" (Filename.basename name) count in
  let fout = open_out "mp" (filename ^ ".mp") in
  let pf fs = ksprintf ( fun s -> fprintf fout "%s" s) fs in
  let () = pf "%s" "
beginfig(1) ;
  u=0.2cm ;
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
