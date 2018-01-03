#ifndef Grille_defined__
#define Grille_defined__ 1

#include "datamodel.h"
#include <fstream>

class Grille {
public:
    struct bar {
      std::vector<Datamodel::Accord> chords_ ;
    };
    struct ligne {
        std::vector<bar> bars_ ;
		int repeat_;
    };
    struct t {
        std::string titre_ ;
        std::vector<ligne> lignes_ ;
    };
    t t_ ; 
    Grille(std::ifstream&,const std::string&) ;
} ;

#endif
