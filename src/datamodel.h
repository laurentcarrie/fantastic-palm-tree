#ifndef datamodel_h_
#define datamodel_h_ 1

namespace Datamodel {

std::string concat_filenames(const std::string& n1,const std::string& n2) ;


static int croche = 8 ;
static int noire = 4 ;
static int blanche = 2 ;
static int ronde = 1 ;


 static std::string builddir ;
 static std::string srcdir ;

 static std::string  tex_silence = 
   // "\\textdagger" 
   "." ;
   
 static std::string tex_idem =  "\\small{\\textdiscount}"  ;

static int int_of_string(const std::string& s) {
    atoi(s.c_str()) ;
}


 class Conf {
 public :
   std::string builddir_ ;
   std::string srcdir_ ;
   std::string song_ ;
 } ;

class Accord {
public:
    enum alteration  { None, Flat, Sharp } ;
    struct c {
        char note_ ;
        alteration alteration_ ;
        bool minor_ ;
        bool minor7_ ;
        bool major7_ ;
        bool diminue_ ;
        bool sus4_ ;
    };
    struct t {
        bool has_position_ ;
        int position_ ;
        bool has_chord_ ;
        c chord_ ;
    };
    t t_ ;
} ;



/*
struct Lyrics {
    struct t {
        int nb_cols_ ;
        std::string title_ ;
        std::vector<std::string> data_ ;
    } ;
} ;
*/

/*

 union context {
 context() : normal("???") {}
 context(const Grille::t&t) : grille(t) {}
 context(std::string s) : normal(s) {}
   std::string normal ;
   std::string titre ;
   std::string auteur ;
   Grille::t grille ;
   Lyrics::t lyrics ;
   Tablature::t tab ;
   std::string mp3 ;
   std::vector<std::string> accords ;
   int transpose ;
   int nb_croches ;
   bool pagebreak ;
   ~context() {}
 } ;
*/

// typedef std::vector<context> document ;


/*
struct Book {
    union s {
        std::string NF ;
        Song::t S ;
    } ;
    struct t {
        std::string filename ;
        std::string titre ;
        std::string auteur ;
        std::vector<s> songs ;
        bool print_index ;
    } ;
} ;

*/

} // namespace

#endif
