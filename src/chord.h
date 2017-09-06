type barre = { from_string : int ; to_string : int ; frette : int }

             type w =
                 | Finger of int
                 | Mute
                 | Open
                 | In_barre of barre

type t = {
b  :
    barre option ;
s1 :
    w ;
s2 :
    w ;
s3 :
    w ;
s4 :
    w ;
s5 :
    w ;
s6 :
    w ;
}


type c = {
name :
    string ;
filename :
    string ;
fingers :
    t ;
}

val transpose :
string -> int -> string

val e_form :
int -> c
val e7_form :
int -> c
val e7M_form :
int -> c
val em_form :
int -> c
val em7_form :
int -> c

val c_form :
int -> c
val c7_form :
int -> c
val c7M_form :
int -> c
val cm_form :
int -> c
val cm7_form :
int -> c

val a_form :
int -> c
val a7_form :
int -> c
val a7M_form :
int -> c
val am_form :
int -> c
val am7_form :
int -> c

val g_form :
int -> c
val g7_form :
int -> c
val g7M_form :
int -> c
val gm_form :
int -> c
val gm7_form :
int -> c


val write_svg :
string -> c -> unit

