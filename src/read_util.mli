val open_out : string -> string -> out_channel
val open_in : string -> string -> in_channel


module Grille : sig
  open Datamodel.Grille
  val bars_of_string : string -> bar list 
end

val read_string_until_empty_line : in_channel -> string
val read_array_until_empty_line : in_channel -> string list

val chord_of_string : string -> Datamodel.Accord.c
val position_and_silence_or_chord_of_string : string -> Datamodel.Accord.t
val bar_of_string : string -> Datamodel.Tablature.bar
val tablature_of_string_list : string -> string list -> Datamodel.Tablature.t
