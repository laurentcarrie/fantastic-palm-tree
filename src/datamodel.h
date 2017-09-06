#ifndef datamodel_h_
#define datamodel_h_ 1

namespace Datamodel {

std::string concat_filenames(const std::string& n1,const std::string& n2) ;


static int croche = 8 ;
static int noire = 4 ;
static int blanche = 2 ;
static int ronde = 1 ;

static std::string tex_idem =  "\\small{\\textdiscount}"  ;

static int int_of_string(const std::string& s) {
    atoi(s.c_str()) ;
}

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
} ;

class Grille {
public:
    struct bar {
        std::vector<Accord::t> chords ;
    };
    struct ligne {
        std::vector<bar> bars ;
    };
    struct t {
        std::string titre ;
        std::vector<ligne> lignes ;
    };
} ;


struct Tablature {
    struct note {
        int frette ;
        int corde ;
    } ;
    struct paquet  {
        std::vector<note> notes ;
        Accord::t chord ;
    } ;
    struct bar {
        std::vector<paquet> paquets ;
    } ;
    struct line {
        std::vector<bar> bars ;
    } ;
    struct t {
        std::string titre ;
        std::vector<line> lines ;
    };
} ;


struct Lyrics {
    struct t {
        int nb_cols ;
        std::string title ;
        std::vector<std::string> data ;
    } ;
} ;

union context {
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

typedef std::vector<context> document ;

struct Song {
    struct t {
        int transpose ;
        std::string filename ;
        std::string titre ;
        std::string auteur ;
        std::vector<context> data ;
        int nb_croches ; /* nombre de croches par mesure : 4/4 -> 8 ; 4/6 -> 12 */
    } ;
} ;

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

}
#endif
