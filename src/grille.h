#ifndef Grille_defined__
#define Grille_defined__ 1

#include "datamodel.h"
#include <fstream>

class Grille {
public:
    struct bar {
      std::vector<Datamodel::Accord::t> chords ;
    };
    struct ligne {
        std::vector<bar> bars ;
    };
    struct t {
        std::string titre_ ;
        std::vector<ligne> lignes_ ;
    };
    t t_ ; 
    Grille(std::ifstream&,const std::string&) ;
} ;

#endif
