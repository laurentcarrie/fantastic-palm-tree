type barre = { from_string : int ; to_string : int ; frette : int }

type w =
    | Finger of int
    | Mute
    | Open
    | In_barre of barre

type t = {
  b  : barre option ;
  s1 : w ;
  s2 : w ;
  s3 : w ;
  s4 : w ;
  s5 : w ;
  s6 : w ;
}


type c = {
  name : string ;
  fingers : t ;
}

val e_form : c

val c_form : c
  
    
val write_svg : string -> c -> unit
      
