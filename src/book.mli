val read : filename:string -> top_src_dir:string -> Datamodel.Book.t
val write : book:Datamodel.Book.t -> unit
val print_deps : book:Datamodel.Book.t -> top_src_dir:string -> top_build_dir:string -> unit
