val write : song:Datamodel.Song.t -> prefix:string -> unit
val read  : filename:string -> Datamodel.Song.t
val print_deps : song:Datamodel.Song.t -> top_build_dir:string -> unit
